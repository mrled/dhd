#!/bin/bash

# DOESN'T WORK YET
# See also: 
# 1. https://www.privateinternetaccess.com/forum/discussion/1151/pia-iptables-manager-new
# 2. http://pastie.org/8164214

#!/bin/bash
set -eu

VPNSERVER="nullyork.younix.us"
VPNSERVER_IP=$(nslookup $VPNSERVER | grep 'Address: ' | cut -d' ' -f2)

iptables -F

iptables -P FORWARD DROP
iptables -P OUTPUT DROP
iptables -P INPUT DROP

iptables -A INPUT -i tun+ -j ACCEPT
iptables -A OUTPUT -o tun+ -j ACCEPT
iptables -A INPUT -s 127.0.0.1 -j ACCEPT
iptables -A OUTPUT -d 127.0.0.1 -j ACCEPT
iptables -A INPUT -s $VPNSERVER_IP -j ACCEPT
iptables -A OUTPUT -d $VPNSERVER_IP -j ACCEPT
