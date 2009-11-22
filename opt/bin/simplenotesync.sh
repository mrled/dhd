#!/bin/bash

# REQUIRES SIMPLENOTESYNC TO BE INSTALLED & WORKING
# Requires extant git repository in your notes directory
# Make sure you set up a .gitignore that includes:
# * simplenotesync.db
# * Notes & Settings
# * .Notational_Write_Ahead_Log
# * .DS_Store
# * ._*
# Written for AndrAIa (Mac OS X)

# make sure this is the right perl w/ the right cpan modules installed
myperl="/usr/bin/perl"
# path to simplenotesync script
mysns="${HOME}/opt/src/SimplenoteSync/SimplenoteSync.pl"
# path to your notes directory 
mynotes="${HOME}/Documents/SimpleNotational"
# path to git just in case
mygit="/opt/local/bin/git"

# Test the network connection
mypingstatus=`/sbin/ping -c1 google.com`
if [ `echo $mypingstatus | grep -c ', 0.0% packet loss'` -gt 0 ]; then
    $myperl "$mysns"
    cd "$mynotes"
    $mygit add *.txt
    mygitstatus=`$mygit status`
    if [ `echo $mygitstatus | grep -c 'nothing to commit (working directory clean)'` -eq 0 ]; then
        $mygit commit -a -m "AUTOCOMMIT: SimplenoteSync complete"
    fi
fi