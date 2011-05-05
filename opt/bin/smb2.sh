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
            _smbclientopts="$_smbclientopts -D \"$remotepath\""
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
        _smbclientopts="$_smbclientopts --user=$username"
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
_put() {
}
_get() {
}
_cp() {
    # figure out which argument is remote
    echo $1 | grep : >/dev/null
    if [ "$?" -eq "0" ]; then arg1remote=1; fi
    echo $2 | grep : >/dev/null
    if [ "$?" -eq "0" ]; then arg2remote=1; fi

    if [ -z $arg1remote ] && [ -z $arg2remote ]; then # both are remote
        echo "When copying files, you cannot copy from a remote server to a remote"
        echo "server due to a limitation of smbclient."
        exit
    elif ! [ -z $arg1remote ] && ! [ -z $arg2remote ]; then # neither are remote
        echo "You are using the cp function, but have not provided any remote URLs"
        echo "to copy to or from."
        echo $URLFORMAT
        exit
    elif ! [ -z $arg1remote ]; then #arg1 is remote; we are downloading
        remotefile="$2"
        _parse "$1"
        $_smbclient $_smbclientopts //$server/$share <<EOF
prompt
mget $remotefile
exit
EOF
    elif ! [ -z $arg2remote ]; then #arg2 is remote; we are uploading
        localfile="$1"
        _parse "$2"
        $_smbclient $_smbclientopts //$server/$share <<EOF
prompt
mput $localfile
exit
EOF
    else
        echo "Something must have gone wrong; we should never have gotten here."
        exit
    fi

    return
}
_ls() {
    _parse $*
    if [ -z "$share" ]; then #no share, just list shares
        $_smbclient $_smbclientopts -L $server
    else #there is a remotepath, view it
        $_smbclient $_smbclientopts //$server/$share <<EOF
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
