#!/bin/bash

cmd_aox=/usr/local/archiveopteryx/bin/aox
cmd_aoximport=/usr/local/archiveopteryx/bin/aoximport


function check_if_maildir_empty {
    maildir = "$1"
    for zuh in "$maildir"; do
        [ "$(ls -A $maildir/cur)" ] && return 1
        [ "$(ls -A $maildir/new)" ] && return 1
    done
    return 0
}

function import_maildir {
    $aox_bin add mailbox
}

cd /home
for user in *; do
sudo /usr/local/archiveopteryx/bin/aox add mailbox /users/neuric/delta-complete-before-nuking/$user neuric
aoximport /users/neuric/delta-complete-before-nuking/$user maildir $user/Maildir
done
