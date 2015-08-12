#!/bin/bash
set -eu

# NOTE: THIS ONLY WORKS FOR LINUX CLIENTS
# from: https://www.agwa.name/blog/post/hardening_openvpn_for_def_con

VPNSERVER="nullyork.younix.us"
VPNSERVER_IP=$(nslookup $VPNSERVER | grep 'Address: ' | cut -d' ' -f2)
OVPN_ROUTE_TABLE="94"

cat > /etc/openvpn/ip-rules.sh <<EOF
ip rule add to ${VPNSERVER_IP} table main pref 1000
ip rule add to ${VPNSERVER_IP} unreachable pref 1001
ip rule add table ${OVPN_ROUTE_TABLE} pref 1002
ip rule add unreachable pref 1003
EOF
chmod 700 /etc/openvpn/ip-rules.sh
grep '^/etc/openvpn/ip-rules.sh' /etc/rc.local > /dev/null || {
    /bin/echo -e "\n/etc/openvpn/ip-rules.sh\n" >> /etc/rc.local
}

cat > /etc/openvpn/route-up.sh <<EOF

EOF
chmod 700 /etc/openvpn/route-up.sh


setenv OPENVPN_ROUTE_TABLE ${OVPN_ROUTE_TABLE}
route-noexec
route-up /etc/openvpn/route-up.sh
route 0.0.0.0 0.0.0.0
EOF
