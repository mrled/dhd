#!/bin/sh
# claudebox-entrypoint: network guard + privilege drop.
#
# In every mode (requires root + NET_ADMIN, which claudebox2 always passes):
# nftables blocks direct egress into private/link-local address space -- the
# container host (host.docker.internal / host.containers.internal), its LAN,
# CGNAT/tailnets, other containers -- with a port-53 carve-out for the
# resolv.conf nameservers, which rootless engines place on such addresses.
#
# Restricted mode (CLAUDEBOX_NETRESTRICT set, a netwhitelist.txt mounted at
# /run/claudebox/netwhitelist.txt, or hostports.txt configured) additionally:
#   - nftables drops all outbound traffic except loopback and the squid user
#     (which also may not reach the private ranges, DNS aside)
#   - squid (127.0.0.1:3128) allows egress only to whitelisted domains
#     (defaults from /etc/claudebox/netwhitelist-default.txt plus the mounted
#     netwhitelist.txt), advertised to CMD via HTTP(S)_PROXY; squid itself
#     refuses private-range destinations so a whitelisted domain that resolves
#     to the host (DNS rebinding) still can't get there
#
# CLAUDEBOX_HOST_PORTS (set by claudebox2 from hostports.txt) allows restricted
# HTTP/CONNECT through squid to host.docker.internal on listed TCP ports.
# nftables pins the exception to its IPv4 gateway address; all other private
# destinations remain blocked.
#
# CMD runs as claude via setpriv. Started without root, restricted mode is an
# error; unrestricted mode execs CMD directly with no guard, warns, and
# reports CLAUDEBOX_NETWORK=unguarded.
set -e

USER_WHITELIST=/run/claudebox/netwhitelist.txt

restricted=
if [ -n "$CLAUDEBOX_NETRESTRICT" ] || [ -f "$USER_WHITELIST" ] || [ -n "${CLAUDEBOX_HOST_PORTS:-}" ]; then
    restricted=1
fi

if [ "$(id -u)" -ne 0 ]; then
    if [ -n "$restricted" ]; then
        echo "claudebox-entrypoint: network restriction requires starting as root with NET_ADMIN" >&2
        exit 1
    fi
    # No root -> can't program the egress guard; run unguarded rather than
    # fail, but say so and report a distinct mode: this path has full network
    # access including the container host.
    echo "claudebox-entrypoint: not started as root; network guard NOT active" \
        "(start with --user root --cap-add NET_ADMIN, or via claudebox2)" >&2
    export CLAUDEBOX_NETWORK=unguarded
    exec "$@"
fi

# RFC1918 + link-local (v4/v6) + CGNAT (also Tailscale) + IPv6 ULA: everything
# that could reach the container host or its networks directly.
PRIVATE4="10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16, 169.254.0.0/16, 100.64.0.0/10"
PRIVATE6="fc00::/7, fe80::/10"

host_service_rule=
host_ports=${CLAUDEBOX_HOST_PORTS:-}
if [ -n "$host_ports" ]; then
    case "$host_ports" in
        *[!0-9,]*|,*|*,|*,,*)
            echo "claudebox-entrypoint: invalid host ports" >&2; exit 1 ;;
    esac
    remaining=$host_ports
    squid_ports=
    nft_ports=
    while [ -n "$remaining" ]; do
        port=${remaining%%,*}
        if [ "$remaining" = "$port" ]; then
            remaining=
        else
            remaining=${remaining#*,}
        fi
        if [ "${#port}" -gt 5 ] || [ "$port" -lt 1 ] || [ "$port" -gt 65535 ]; then
            echo "claudebox-entrypoint: invalid host port: $port" >&2
            exit 1
        fi
        squid_ports="$squid_ports $port"
        nft_ports="${nft_ports:+$nft_ports, }$port"
    done
    # --add-host=host.docker.internal:host-gateway is passed by claudebox2.
    host_service_ip=$(getent ahostsv4 host.docker.internal | awk 'NR == 1 { print $1 }')
    if [ -z "$host_service_ip" ]; then
        echo "claudebox-entrypoint: cannot resolve host.docker.internal" >&2
        exit 1
    fi
fi

# Emit per-nameserver port-53 accept rules, each prefixed with $1 (e.g. a
# skuid match). The resolver rootless podman/docker provides usually sits in a
# private range, so without these the private-range drops would kill all DNS.
dns_accepts() {
    awk -v pre="$1" '/^nameserver[ \t]/ {
        fam = ($2 ~ /:/) ? "ip6" : "ip"
        printf "        %s %s daddr %s udp dport 53 accept\n", pre, fam, $2
        printf "        %s %s daddr %s tcp dport 53 accept\n", pre, fam, $2
    }' /etc/resolv.conf
}

if [ -n "$restricted" ]; then
    # Combine default + user whitelists, passing entries to squid dstdomain
    # as written: 'host.com' matches that host exactly, '.host.com' matches the
    # domain and all subdomains. One entry per line; blank lines and # comments
    # are ignored.
    mkdir -p /run/claudebox
    combined=$(mktemp)
    cat /etc/claudebox/netwhitelist-default.txt > "$combined"
    if [ -f "$USER_WHITELIST" ]; then
        cat "$USER_WHITELIST" >> "$combined"
    fi
    sed -e 's/#.*//' -e 's/[[:space:]]//g' -e '/^$/d' \
        "$combined" > /run/claudebox/whitelist.txt
    rm -f "$combined"
    if [ -n "$host_ports" ]; then
        echo host.docker.internal >> /run/claudebox/whitelist.txt
    fi

    # Only loopback and squid's worker uid may send packets -- and even squid
    # may not enter the private ranges (its resolver excepted); everything
    # else in the container has no direct route out.
    proxy_uid=$(id -u proxy)
    if [ -n "$host_ports" ]; then
        host_service_rule="        meta skuid $proxy_uid ip daddr $host_service_ip tcp dport { $nft_ports } accept"
    fi
    nft -f /dev/stdin <<EOF
table inet claudebox {
    chain output {
        type filter hook output priority 0; policy drop;
        oifname "lo" accept
$(dns_accepts "meta skuid $proxy_uid")
$host_service_rule
        meta skuid $proxy_uid ip daddr { $PRIVATE4 } drop
        meta skuid $proxy_uid ip6 daddr { $PRIVATE6 } drop
        meta skuid $proxy_uid accept
    }
}
EOF

    # Pre-create the logs world-readable so the claude user can inspect them
    # (see netblocked); squid runs as proxy and preserves existing perms.
    mkdir -p /var/log/squid
    touch /var/log/squid/access.log /var/log/squid/cache.log
    chown -R proxy:proxy /var/log/squid
    chmod 755 /var/log/squid
    chmod 644 /var/log/squid/access.log /var/log/squid/cache.log

    squid_config=/etc/claudebox/squid.conf
    if [ -n "$host_ports" ]; then
        squid_config=/run/claudebox/squid.conf
        # Docker also advertises an IPv6 gateway that is not reachable from
        # this container. Give Squid only the IPv4 gateway for this hostname;
        # nftables pins the actual connection to that same IP and port.
        printf '%s %s\n' "$host_service_ip" host.docker.internal > /run/claudebox/host-service-hosts
        chmod 644 /run/claudebox/host-service-hosts
        awk -v ports="$squid_ports" '
            BEGIN { print "hosts_file /run/claudebox/host-service-hosts" }
            /^http_access deny CONNECT !SSL_ports$/ {
                print "acl host_service_host dstdomain host.docker.internal"
                print "acl host_service_port port " ports
                # Pi tunnels even HTTP requests using CONNECT, so this narrow
                # allow must precede the general non-443 CONNECT denial.
                print "http_access allow host_service_host host_service_port whitelisted"
            }
            { print }
        ' /etc/claudebox/squid.conf > "$squid_config"
    fi
    squid -f "$squid_config"

    # squid daemonizes before it listens; any HTTP response (even an error page)
    # means it's up.
    tries=0
    until curl -s -o /dev/null --max-time 2 http://127.0.0.1:3128/; do
        tries=$((tries + 1))
        if [ "$tries" -ge 50 ]; then
            echo "claudebox-entrypoint: squid did not start listening on 127.0.0.1:3128" >&2
            exit 1
        fi
        sleep 0.2
    done

    export CLAUDEBOX_NETWORK=restricted
    proxy_url=http://127.0.0.1:3128
    export http_proxy="$proxy_url" https_proxy="$proxy_url"
    export HTTP_PROXY="$proxy_url" HTTPS_PROXY="$proxy_url"
    export no_proxy=localhost,127.0.0.1 NO_PROXY=localhost,127.0.0.1
else
    # Unrestricted: full egress except straight into private space. The ICMPv6
    # accept must precede the drops or neighbor/router discovery (which targets
    # link-local addresses) breaks and IPv6 dies entirely.
    nft -f /dev/stdin <<EOF
table inet claudebox {
    chain output {
        type filter hook output priority 0; policy accept;
        oifname "lo" accept
$(dns_accepts "")
        icmpv6 type { nd-router-solicit, nd-neighbor-solicit, nd-neighbor-advert } accept
        ip daddr { $PRIVATE4 } drop
        ip6 daddr { $PRIVATE6 } drop
    }
}
EOF

    export CLAUDEBOX_NETWORK=unrestricted
fi

exec setpriv --reuid=claude --regid=claude --init-groups \
    env HOME=/home/claude USER=claude LOGNAME=claude "$@"
