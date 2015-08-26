#!/bin/sh
set -eu

# Younix OpenVPN hardening system. See {{ openvpn_hardened_readme_path }}

VPNSERVER_IP="{{ openvpn_server.ipaddress }}"
OVPN_ROUTE_TABLE="{{ openvpn_hardened_route_table }}"

[ -z "$VPNSERVER_IP" ]     && (/bin/echo "openvpn_server.ipaddress variable not supplied"           && exit 1)
[ -z "$OVPN_ROUTE_TABLE" ] && (/bin/echo "opnvpn_hardened_route_table variable not supplied" && exit 1)

ip rule add to ${VPNSERVER_IP} table main pref 1000
ip rule add to ${VPNSERVER_IP} unreachable pref 1001
ip rule add table ${OVPN_ROUTE_TABLE} pref 1002
ip rule add unreachable pref 1003
