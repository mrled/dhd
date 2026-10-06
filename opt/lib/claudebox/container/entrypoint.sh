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
# /run/claudebox/netwhitelist.txt) additionally:
#   - nftables drops outbound traffic except loopback, IPv6 neighbor discovery,
#     and the squid user
#     (which also may not reach the private ranges, DNS aside)
#   - squid (127.0.0.1:3128) allows egress only to whitelisted domains
#     (defaults from /etc/claudebox/netwhitelist-default.txt plus the mounted
#     netwhitelist.txt), advertised to CMD via HTTP(S)_PROXY; squid itself
#     refuses private-range destinations so a whitelisted domain that resolves
#     to the host (DNS rebinding) still can't get there
#
# CLAUDEBOX_HOST_PORTS (set by claudebox2 from hostports.txt) allows
# HTTP/CONNECT through squid to host.docker.internal / host.containers.internal
# on listed TCP ports, without enabling public-domain restriction.
# nftables pins the exception to its IPv4 gateway address; all other private
# destinations remain blocked.
#
# CMD runs as claude via setpriv. Started without root, restriction or hostports
# is an error; plain unrestricted mode execs CMD directly with no guard, warns, and
# reports CLAUDEBOX_NETWORK=unguarded.
set -e

USER_WHITELIST=/run/claudebox/netwhitelist.txt

restricted=
if [ -n "${CLAUDEBOX_NETRESTRICT:-}" ] || [ -f "$USER_WHITELIST" ]; then
    restricted=1
fi

if [ "$(id -u)" -ne 0 ]; then
    if [ -n "$restricted" ] || [ -n "${CLAUDEBOX_HOST_PORTS:-}" ]; then
        echo "claudebox-entrypoint: network restriction or hostports requires starting as root with NET_ADMIN" >&2
        exit 1
    fi
    # No root -> can't program the egress guard; run unguarded rather than
    # fail, but say so and report a distinct mode: this path has full network
    # access including the container host.
    echo "claudebox-entrypoint: not started as root; network guard NOT active" \
        "(start with --user root --cap-add NET_ADMIN, or via claudebox)" >&2
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
    # Docker gets an explicit host-gateway alias from claudebox2; Podman
    # supplies its own backend-specific aliases (also on Podman Machine).
    host_service_ip=$(getent ahostsv4 host.docker.internal | awk 'NR == 1 { print $1 }')
    if [ -z "$host_service_ip" ]; then
        host_service_ip=$(getent ahostsv4 host.containers.internal | awk 'NR == 1 { print $1 }')
    fi
    if [ -z "$host_service_ip" ]; then
        echo "claudebox-entrypoint: cannot resolve an IPv4 container host address (host.docker.internal / host.containers.internal)" >&2
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
fi

# Restricted: only loopback, neighbor discovery and squid may send packets.
# Unrestricted (including hostports-only): all uids may reach public addresses,
# but private drops apply to all uids. Only squid gets the exact host-port
# exception, before those drops. DNS is limited to the configured resolvers.
policy=accept
dns_prefix=
private_prefix=
proxy_accept=
export CLAUDEBOX_NETWORK=unrestricted
if [ -n "$restricted" ]; then
    policy=drop
    proxy_uid=$(id -u proxy)
    dns_prefix="meta skuid $proxy_uid"
    private_prefix="meta skuid $proxy_uid "
    proxy_accept="        meta skuid $proxy_uid accept"
    export CLAUDEBOX_NETWORK=restricted
fi
if [ -n "$host_ports" ]; then
    proxy_uid=$(id -u proxy)
    host_service_rule="        meta skuid $proxy_uid ip daddr $host_service_ip tcp dport { $nft_ports } accept"
fi
# Narrow ICMPv6 allowance keeps neighbor/router discovery working in both modes.
nft -f /dev/stdin <<EOF
table inet claudebox {
    chain output {
        type filter hook output priority 0; policy $policy;
        oifname "lo" accept
$(dns_accepts "$dns_prefix")
        icmpv6 type { nd-router-solicit, nd-neighbor-solicit, nd-neighbor-advert } accept
$host_service_rule
        ${private_prefix}ip daddr { $PRIVATE4 } drop
        ${private_prefix}ip6 daddr { $PRIVATE6 } drop
$proxy_accept
    }
}
EOF

if [ -n "$restricted" ] || [ -n "$host_ports" ]; then
    mkdir -p /run/claudebox
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
        # Give both aliases only the selected IPv4 host address, avoiding
        # unreachable IPv6 gateways. nftables pins the actual connection to
        # that same IP and port, regardless of the runtime's DNS aliases.
        printf '%s %s %s\n' "$host_service_ip" host.docker.internal host.containers.internal > /run/claudebox/host-service-hosts
        chmod 644 /run/claudebox/host-service-hosts
        awk -v ports="$squid_ports" -v restricted="$restricted" '
            BEGIN { print "hosts_file /run/claudebox/host-service-hosts" }
            /^http_access deny CONNECT !SSL_ports$/ {
                print "acl host_service_host dstdomain host.docker.internal host.containers.internal"
                print "acl host_service_port port " ports
                # Pi tunnels even HTTP requests using CONNECT, so this narrow
                # allow must precede the general non-443 CONNECT denial.
                print "http_access allow host_service_host host_service_port"
                print "http_access deny host_service_host"
                if (!restricted) next
            }
            !restricted && /^acl whitelisted / { next }
            !restricted && /^http_access allow whitelisted$/ {
                print "http_access allow all"
                next
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

    proxy_url=http://127.0.0.1:3128
    export http_proxy="$proxy_url" https_proxy="$proxy_url"
    export HTTP_PROXY="$proxy_url" HTTPS_PROXY="$proxy_url"
    export no_proxy=localhost,127.0.0.1 NO_PROXY=localhost,127.0.0.1
fi

exec setpriv --reuid=claude --regid=claude --init-groups \
    env HOME=/home/claude USER=claude LOGNAME=claude "$@"
