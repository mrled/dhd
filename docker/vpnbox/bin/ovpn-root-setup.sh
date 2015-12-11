#!/bin/sh

set -e
set -u

OVPNDIR=$1
OVPNUSER=$2
OVPNSUBNET=$3

OVPNDIR="/etc/openvpn"
LOGFILE="${OVPNDIR}/ovpn-root-setup.log"
touch ${LOGFILE}
exec 3>&1 4>&2 >${LOGFILE} 2>&1

id ${OVPNUSER} >/dev/null 2>&1  || { echo "OpenVPN service user '${OVPNUSER}' does not exist"; exit 1; };
[ -z "${OVPNSUBNET}" ]          && { echo "There is no subnet specified"; exit 1; }

log() {
    date=`date --rfc-3339=seconds`
    message="$date $*"
    echo "$message"
}

# NOTE: Older instructions will tell you to "modprobe tun" before running the 
# openvpn --mktun command. For newer kernels, tun support appears to be built 
# in, meaning modprobe won't work but this will:
if [ -d /dev/net ]; then 
    log "/dev/net exists"
else 
    log "Creating /dev/net directory"
    mkdir -p /dev/net
fi
if [ -c /dev/net/tun ]; then
    log "/dev/net/tun already exists"
else
    log "Creating /dev/net/tun with mknod"
    mknod /dev/net/tun c 10 200
fi

log "Using OpenVPN to make the tun1194 device"
openvpn --mktun --dev tun1194 --dev-type tun --user ${OVPNUSER} --group ${OVPNUSER}

#sysctl -w net.ipv4.ip_forward=1
log "Using OpenVPN to make the tun1194 device"
iptables -t nat -A POSTROUTING -s ${OVPNSUBNET}/24 -o eth0 -j MASQUERADE

log "Creating the OpenVPN server log"
touch openvpn-server.log
log "Starting OpenVPN..."

# Undo the output redirection:
exec 1>&3 2>&4
