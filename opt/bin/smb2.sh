#!/bin/bash

smbclient=`env smbclient`
thiscmd=`basename $0`
USAGE="Type '$thiscmd help' for help."

_parse() {
    serverstring="$*"
    # expecting such strings as:
    # - user@server
    # - user@server:share
    # - user@server:share/path
    # - server
    # - server:share
    # - server:share/path

    echo $serverstring | grep \@ >/dev/null
    if [ "$?" -eq "0" ]; then #username present
        username=`echo $serverstring | awk 'BEGIN {FS="@"}; {print $1}'`
        serverstring=`echo $serverstring | awk 'BEGIN {FS="@"}; {print $2}'`
    fi
    # now string will look like this: 
    # - server
    # - server:share
    # - server:share/path
    echo $serverstring | grep : >/dev/null
    if [ "$?" -eq "0" ]; then #share present
        server=`echo $serverstring | awk 'BEGIN {FS=":"}; {print $1}'`
        serverstring=`echo $serverstring | awk 'BEGIN {FS=":"}; {print $2}'`
    fi
    # now string will look like this:
    # - share
    # - share/path
    echo $serverstring | grep \/ >/dev/null 
    if [ "$?" -eq "0" ]; then #path present
        share=`echo  $serverstring | awk 'BEGIN {FS="/"}; {print $1}'`
        remotepath=`echo $serverstring | sed "s/$share\///"`
    else
        share=$serverstring
    fi
}
_echo() {
    _parse $*
    echo "User:  $username"
    echo "Host:  $server"
    echo "Share: $share"
    echo "Path:  $remotepath"
}
_cp() {
    return
}
_ls() {
    return
}
_help() {
    #echo $USAGE
    echo "More help available soon lol"
}

# first parse the main command

case $1 in 
    "ls")   shift; _ls   $*; exit;;
    "cp")   shift; _cp   $*; exit;;
    "echo") shift; _echo $*; exit;;

    "help") shift; _help; exit;;
    *) echo $USAGE; exit;;
esac
