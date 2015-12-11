#!/bin/bash

set -e
set -u

export OVPNDIR="/etc/openvpn"
[ -f "${OVPNDIR}/server.ovpn" ] || { echo "You have no OpenVPN configuration file"; exit 1; }; 
[ -z "${OVPNSUBNET}" ]          && { echo "There is no subnet specified"; exit 1; }

OVPNUSER=`whoami`
sudo /usr/local/sbin/ovpn-root-setup.sh "${OVPNDIR}" "${OVPNUSER}" "${OVPNSUBNET}"
cd ${OVPNDIR}
touch openvpn-server.log
openvpn server.ovpn | tee --append openvpn-server.log 
#while true ; do openvpn server.ovpn ; done >> openvpn-server.log &
