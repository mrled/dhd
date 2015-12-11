#!/bin/sh
# 1. Run OpenVPN as non-root
# 2. Let that user run the 'ip' command through sudo without a password:
#    echo '${OVPNUSER} ALL=(ALL) NOPASSWD: /sbin/ip' > /etc/sudoers.d/openvpn
# 3. Tell it to use this command by adding this line to the server config file:
#    iproute /usr/local/sbin/sudo-ip.sh
sudo /sbin/ip $*
