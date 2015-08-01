#!/bin/sh
set -eu

get_abs_path() {
    # Q: Why does this work? A: http://stackoverflow.com/a/21188136/868206
    # NOTE: won't work for executables in $PATH! 
    echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
}
SCRIPTPATH=`get_abs_path "$0"`
SCRIPTROOT=`dirname "$SCRIPTPATH"`
SCRIPTNAME=`basename "$SCRIPTPATH"`
PACKFILE="$SCRIPTROOT/vpn-client-config.tar"
TMPDIR="$SCRIPTROOT/vpn-client-config"
PKI="$SCRIPTROOT/pki"

show_usage() {
    echo "$SCRIPTNAME: package client configuration"
    echo "Uses a temporary directory at '$TMPDIR'"
    echo "Creates a package file at '$PACKFILE'"
    echo "$SCRIPTNAME [force]"
    echo "OPTIONS:"
    echo "    force: remove the temp dir and pack file, if they exist"
}

OVERWRITE=""
[ $# -gt 1 ] && show_usage && exit 1
[ $# -gt 0 ] && {
    case $1 in
        "force") OVERWRITE="true";;
        *)       show_usage; exit;;
    esac
}


if [ -z "$OVERWRITE" ]; then 
    [ -f "$PACKFILE" ] && echo "Client config already packed at '$PACKFILE'" && exit 1
    [ -d "$TMPDIR"   ] && echo "Temp directory already exists '$TMPDIR'"     && exit 1
else
    [ -f "$PACKFILE" ] && rm -f "$PACKFILE"
    [ -d "$TMPDIR"   ] && rm -rf "$TMPDIR"
fi

mkdir -p "$TMPDIR"
cp "$SCRIPTROOT/conf/client.ovpn" "$TMPDIR/vpn.ovpn"
for file in "$PKI/ca.crt" "$PKI/issued/genericclient.crt" "$PKI/private/genericclient.key" "$PKI/private/dh.pem" "$PKI/private/ta.key"; do
    cp "$file" "$TMPDIR"
done

pushd "$TMPDIR" > /dev/null
tar -f "$PACKFILE" -c *
popd > /dev/null

rm -rf "$TMPDIR"