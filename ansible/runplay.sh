#!/bin/sh

set -e
set -u
set -v

environment=$1
invfile="inventory/$environment"
vpfile=".vault-pass-$environment"
echo $invfile
test -e "$invfile" || $(echo "No environment at '$invfile'" && exit 1)
test -e "$vpfile"  || $(echo "No vault password file at '$vpfile'" && exit 1)
shift 1
ansible-playbook --inventory-file="$invfile" --vault-password-file="$vpfile" $*
