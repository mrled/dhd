#!/bin/sh
# claudebox-entrypoint: passthrough exec unless network restriction is
# requested (CLAUDEBOX_NETRESTRICT set, or a netwhitelist.txt mounted at
# /run/claudebox/netwhitelist.txt). When restricted:
#   - nftables drops all outbound traffic except loopback and the squid user
#   - squid (127.0.0.1:3128) allows egress only to whitelisted domains
#     (defaults from /etc/claudebox/netwhitelist-default.txt plus the mounted
#     netwhitelist.txt), advertised to CMD via HTTP(S)_PROXY
# Restricted mode must start as root with NET_ADMIN (claudebox2 passes
# --user root --cap-add NET_ADMIN); CMD still runs as claude via setpriv.
set -e

USER_WHITELIST=/run/claudebox/netwhitelist.txt

if [ -z "$CLAUDEBOX_NETRESTRICT" ] && [ ! -f "$USER_WHITELIST" ]; then
    exec "$@"
fi

if [ "$(id -u)" -ne 0 ]; then
    echo "claudebox-entrypoint: network restriction requires starting as root with NET_ADMIN" >&2
    exit 1
fi

# Combine default + user whitelists into squid dstdomain form; a leading dot
# matches the domain itself and all subdomains. Entries are domains, one per
# line; blank lines and # comments are ignored.
mkdir -p /run/claudebox
combined=$(mktemp)
cat /etc/claudebox/netwhitelist-default.txt > "$combined"
if [ -f "$USER_WHITELIST" ]; then
    cat "$USER_WHITELIST" >> "$combined"
fi
sed -e 's/#.*//' -e 's/[[:space:]]//g' -e '/^$/d' -e 's/^\.//' -e 's/^/./' \
    "$combined" > /run/claudebox/whitelist.txt
rm -f "$combined"

# Only loopback and squid's worker uid may send packets; everything else in
# the container has no direct route out.
nft -f /dev/stdin <<EOF
table inet claudebox {
    chain output {
        type filter hook output priority 0; policy drop;
        oifname "lo" accept
        meta skuid $(id -u proxy) accept
    }
}
EOF

squid -f /etc/claudebox/squid.conf

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

exec setpriv --reuid=claude --regid=claude --init-groups \
    env HOME=/home/claude USER=claude LOGNAME=claude "$@"
