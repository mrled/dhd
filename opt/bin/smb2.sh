#!/bin/bash

_smbclient=smbclient
_smbclientopts=
#_smbclientopts="--authentication-file=~mledbetter/.netrc.smbclient"
thiscmd=`basename $0`
USAGE="Usage: '$thiscmd [cp|ls|help|echo]' [URL]."
URLFORMAT="URLs must be in form [user@]host[:share[/path/to/somewhere]]."

_parse() {
    wholestring="$*"
    # expecting such strings as:
    # - user@server
    # - user@server:share
    # - user@server:share/path
    # - server
    # - server:share
    # - server:share/path

    # first determine if we have to parse a ":share[/path]" string
    echo $wholestring | grep : >/dev/null
    if [ "$?" -eq "0" ]; then #"share[/path]" present
        userserver=`echo $wholestring | awk 'BEGIN {FS=":"}; {print $1}'`
        sharepath=`echo $wholestring | awk 'BEGIN {FS=":"}; {print $2}'`
        # $userserver contains something like "user@server" or just "server"
        # $sharepath contains something like "share/path" or just "share"

        # parse the "share[/path]" string. Do we have a path, or just a share?
        echo $sharepath | grep \/ >/dev/null 
        if [ "$?" -eq "0" ]; then #path present
            share=`echo $sharepath | awk 'BEGIN {FS="/"}; {print $1}'`
            remotepath=`echo $sharepath | sed "s/$share\///"`
        else #no path present, so the whole string is just the share
            share=$sharepath
        fi

    else 
        # first make sure the user didn't accidentally give you a url like
        # user@server/share/path, which we don't support yet
        echo $wholestring | grep \/ >/dev/null 
        if [ "$?" -eq "0" ]; then 
            echo "Bad URL. $URLFORMAT"
            exit
        fi
        # with that out of the way: 
        # no "share[/path]" present, so the whole string is just the "[user@]server"
        userserver="$wholestring"
    fi

    # now determine if we have a "user@server" string, or just a "server" string
    echo $userserver | grep \@ >/dev/null
    if [ "$?" -eq "0" ]; then #username present
        username=`echo $userserver | awk 'BEGIN {FS="@"}; {print $1}'`
        server=`echo $userserver | awk 'BEGIN {FS="@"}; {print $2}'`
    else #no username present so the whole string is just the server
        server=$userserver
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
    _parse $*
    if ! [ -z "$username" ]; then #we are not anonymous
        _smbclientopts="$smbclientopts --user=$username"
    fi
    if [ -z "$share" ]; then #no share, just list shares
        $_smbclient $_smbclientopts -L $server
    else #there is a remotepath, view it
        $_smbclient $_smbclientopts //$server/$share -D $remotepath <<EOF
ls
exit
EOF
    fi

    return
}
_help() {
    echo $USAGE
    echo "More help available soon lol"
}

# first parse the main command, use shift to eat that command, and pass the rest of
# the arguments on to the appropriate function. 
case $1 in 
    "ls")   shift; _ls   $*; exit;;
    "cp")   shift; _cp   $*; exit;;
    "echo") shift; _echo $*; exit;;

    "help") shift; _help; exit;;
    *) echo $USAGE; exit;;
esac
