#!/bin/bash

# From http://serverfault.com/a/532860/89561
# Updated for Mavericks
# Uses the first unused uid <= 500 which also has an identical unused gid
# Prepends a _ to the username (to mimic OSX behavior)

if (( $(id -u) )) ; then
    echo "This script needs to run as root"
    exit 1
fi

if [[ -z "$1" ]] ; then
    echo "Usage: $(basename $0) [username] [realname (optional)]"
    exit 1
fi

username=$1
realname="${2:-$username}"

echo "Adding daemon user $username with real name \"$realname\""

for (( uid = 500;; --uid )) ; do
    if ! id -u $uid &>/dev/null; then
        if ! dscl /Local/Default -ls Groups gid | grep -q [^0-9]$uid\$ ; then
          dscl /Local/Default -create Groups/_$username
          dscl /Local/Default -create Groups/_$username Password \*
          dscl /Local/Default -create Groups/_$username PrimaryGroupID $uid
          dscl /Local/Default -create Groups/_$username RealName "$realname"
          dscl /Local/Default -create Groups/_$username RecordName _$username $username

          dscl /Local/Default -create Users/_$username
          dscl /Local/Default -create Users/_$username NFSHomeDirectory /var/empty
          dscl /Local/Default -create Users/_$username Password \*
          dscl /Local/Default -create Users/_$username PrimaryGroupID $uid
          dscl /Local/Default -create Users/_$username RealName "$realname"
          dscl /Local/Default -create Users/_$username RecordName _$username $username
          dscl /Local/Default -create Users/_$username UniqueID $uid
          dscl /Local/Default -create Users/_$username UserShell /usr/bin/false

          dscl /Local/Default -delete /Users/_$username AuthenticationAuthority
          dscl /Local/Default -delete /Users/_$username PasswordPolicyOptions
          break
        fi
    fi
done

echo -e "Created system user $username (uid/gid $uid):\n"

dscl /Local/Default -read Users/_$username

echo -e "\nYou can undo the creation of this user by issuing the following commands:\n"
echo "sudo dscl /Local/Default -delete Users/_$username"
echo "sudo dscl /Local/Default -delete Groups/_$username"
