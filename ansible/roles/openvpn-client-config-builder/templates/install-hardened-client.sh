#!/bin/bash
set -eux

# NOTE: THIS ONLY WORKS FOR LINUX CLIENTS

# This only needs to be run once (but should be idempotent)

# See {{ openvpn_hardened_readme_name }}

ipscript_name="{{ openvpn_hardened_ip_rules_script_name }}"
ipscript_path="{{ openvpn_hardened_ip_rules_script_path }}"
routescript_name="{{ openvpn_hardened_route_script_name }}"
routescript_path="{{ openvpn_hardened_route_script_path }}"
sysctlconf_name="{{ openvpn_hardened_sysctl_conf_name }}"
sysctlconf_path="{{ openvpn_hardened_sysctl_conf_path }}"
resolvconf_name="{{ openvpn_hardened_resolv_conf_name }}"
resolvconf_path="{{ openvpn_hardened_resolv_conf_path }}"
readme_name="{{ openvpn_hardened_readme_name }}"
readme_path="{{ openvpn_hardened_readme_path }}"
ovpn_conf_name="{{ openvpn_hardened_client_conf_name }}"
ovpn_conf_path="{{ openvpn_client_conf_path }}"

#sysctl_d_script_path="/etc/sysctl.d/younix-disable-ipv6.conf"
#resolveconf_d_script_path="/etc/dhcp/dhclient-enter-hooks.d/zzz-younix-preserve-resolvconf"

get_abs_path() {
    echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
}
scriptpath=$(get_abs_path "$0")
scriptroot=$(dirname "$scriptpath")
scriptname=$(basename "$scriptpath")

disable_ipv6() {
    cp "$scriptroot/$sysctlconf_name" "$sysctlconf_path"
    chmod 644 "$sysctlconf_path"
    sysctl -p "$sysctlconf_path"
}

disable_dhcp_dns_updates() {
    cp "$scriptroot/$resolvconf_name" "$resolvconf_path"
    chmod 755 "$resolvconf_path"
}

setup_ip_rules_script() {
    cp "$scriptroot/$ipscript_name" "$ipscript_path"
    chmod 700 "$ipscript_path"
    grep "^$ipscript_path" /etc/rc.local > /dev/null || {
        /bin/echo -e "\n$ipscript_path\n" >> /etc/rc.local
    }
}

setup_ovpn_route_script() {
    cp "$scriptroot/$routescript_name" "$routescript_path"
    chmod 700 "$routescript_path"
}

copy_readme() {
    cp "$scriptroot/$readme_name" "$readme_path"
    chmod 644 "$readme_path"
}

copy_ovpn_config() {
    cp "$scriptroot/$ovpn_conf_name" "$ovpn_conf_path"
    chmod 644 "$ovpn_conf_path"
}

disable_ipv6
disable_dhcp_dns_updates
setup_ip_rules_script
setup_ovpn_route_script
copy_readme
copy_ovpn_config
