#!/bin/bash
# {En,De}crypt the private/ directory
# The private/ directory should not be stored in version control

get_abs_path() {
    # Q: Why does this work? A: http://stackoverflow.com/a/21188136/868206
    echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
}
SCRIPTPATH=`get_abs_path "$0"`
SCRIPTROOT=`dirname "$SCRIPTPATH"`
SCRIPTNAME=`basename "$SCRIPTPATH"`

PRIVATE_DIRNAME="private"
PRIVATE_DIRPATH="$SCRIPTROOT/$PRIVATE_DIRNAME"
PRIVATE_BAK="$SCRIPTROOT/private_bak"
PRIVATE_TAR="$SCRIPTROOT/private.tar"
PRIVATE_ENC="$SCRIPTROOT/private.tar.encrypted.asc"

encrypt_private() {
    # This does no checking because we assume that private.tar.encrypted is in git. 
    tar c -C "$SCRIPTROOT" "$PRIVATE_DIRNAME" > "$PRIVATE_TAR"
    gpg --cipher-algo AES256 --armor --output "$PRIVATE_ENC" --symmetric "$PRIVATE_TAR"
    rm "$PRIVATE_TAR"
}
decrypt_private() {
    if [ -d "$PRIVATE_DIRPATH" ]; then 
        if [ -d "$PRIVATE_BAK" ]; then
            rm -rf "$PRIVATE_BAK"
        fi
        mv "$PRIVATE_DIRPATH" "$PRIVATE_BAK"
    fi
    gpg --output "$PRIVATE_TAR" --decrypt "$PRIVATE_ENC"
    tar xf "$PRIVATE_TAR" -C "$SCRIPTROOT" 
    rm "$PRIVATE_TAR"
}
show_help() {
    echo "$SCRIPTNAME - {en,de}crypt the private/ directory"
    echo "    The private/ directory should not be stored in version control"
    echo "    Usage: $SCRIPTNAME {enc,dec}"
    echo "        enc: encrypt the private/ directory"
    echo "        dec: decrypt the private/ directory"
}

case "$1" in
    enc)
        encrypt_private;;
    dec)
        decrypt_private;;
    *)
        show_help;;
esac
