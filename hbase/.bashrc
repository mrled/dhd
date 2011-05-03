# .bashrc

#. ~/doc/dhd/hbase/.sh_ansi_color

## Set the path
#   - not workable if the directory has spaces
#   - put commands that should come before system commands in {,~}/opt/alternatives/
h="${HOME}"
PATH=
d=
d="${d}/Library/Frameworks/Python.framework/Versions/2.7/bin/python"
d="${d} /usr/local/texlive/2008/bin/universal-darwin"
d="${d} $h/opt/alternatives /opt/alternatives $h/opt/bin $h/opt/sbin"
d="${d} $h/doc/dhd/opt/bin $h/doc/dhd/os/$uname/bin"
d="${d} /sw/bin /sw/sbin /opt/local/bin /opt/local/sbin /Developer/usr/bin /Developer/usr/sbin"
d="${d} /usr/pkg/bin /usr/pkg/sbin"
d="${d} /usr/nekoware/bin /usr/nekoware/sbin /usr/freeware/bin"
d="${d} /opt/csw/bin /opt/csw/sbin /opt/csw/flex/bin /opt/csw/flex/sbin /opt/csw/gcc4/bin"
d="${d} /opt/csw/gcc4/sbin /opt/SUNWspro/bin /opt/SUNWspro/sbin"

# This is all for SFU/SUA, which means I'll probably never need it again.
d="${d} /opt/gcc.3.3/bin/i586-pc-interix3 /usr/local/MSVisualStudio/bin"
d="${d} /opt/gcc.3.3/bin /opt/ast/bin"
d="${d} /usr/contrib/bin /usr/contrib/win32/bin /usr/examples/admin"

#d="${d} /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin"
#d="${d} /usr/mylocal/bin /usr/mylocal/sbin"

d="${d} /usr/local/bin /usr/local/sbin /usr/bin /usr/sbin /bin /sbin"
d="${d} /usr/games /usr/games/bin /usr/X11R6/bin /usr/X11R6/sbin /usr/bin/X11"

# mingw/msys stuff
#d="${d} /mingw/bin /c/WINDOWS /c/WINDOWS/system32/Wbem /c/WINDOWS/system32 /c/opt/bin"
d="${d} /c/WINDOWS /c/WINDOWS/system32/Wbem /c/WINDOWS/system32"
#d="${d} /c/opt/bin /c/opt/sbin"
d="${d} /c/MinGW/bin /c/MinGW/sbin /c/MinGW/msys/1.0/bin /c/MinGW/msys/1.0/sbin"
# Remember: spaces in here won't work even if escaped w/ '\'! 
# I had to make C:\ProgramFiles with Junction.exe from sysinternals.
# d="${d} /c/ProgramFiles/Emacs/emacs/bin"
d="${d} /c/opt/ntemacs24/bin"
d="${d} /c/opt/svn/bin /c/opt/SysinternalsSuite"
# BE CAREFUL: if your C:\opt contains ls and friends from UnxUtils or GnuWin32, 
# you might not want to add it here
d="${d} /c/opt/bin /c/opt/sbin"
# this should go last becausae it has some things that won't work with MinTTY like vim and sh.exe
d="${d} /c/opt/git/bin"

for p in ${d}; do
    if [ -d ${p} ]; then PATH="${PATH}${p}:"; fi
done
export PATH
unset d h
#export MANPATH="${MANPATH}:/opt/local/share/man"

uname=`uname`
host=`hostname`
me=`whoami`
menum=`id -u`

#??export HOME TERM 
umask 077 #stop reading my files!!
export CVS_RSH="ssh"

cmd_ls="ls"
ls_args="-hF"
cmd_sed="sed"
cmd_du="du"
cmd_grep="grep"

#### Annoying inter-OS issues
# obsd & macosx: `/usr/bin/which -a` does the same as tcsh's `whence`
# debian & macosx: `which asdf` returns false; obsd returns "command not found"
#   this makes it impossible to rely on `which` in .bashrc - grr! 

# Found solution: use `whence` on ksh, and either `hash` or `type` on bash

if type -P gls >/dev/null; then 
    cmd_ls=gls
    ls_args="${ls_args} --color"
elif type -P colorls >/dev/null; then 
    cmd_ls=colorls
    ls_args="${ls_args} -G"
fi

if type -P gsed >/dev/null; then cmd_sed=gsed; fi
if type -P gdu  >/dev/null; then cmd_du=gdu;   fi


## Defaults which can be overridden in the system-specific configurations below
psargs="ax"
psargs_user="j"

if [ -d /cygdrive ]; then    # Cygwin
    # it inherits the Windows path, so if your Windows path has this set, 
    #  then it will be wrong:
    export SVN_SSH="/usr/bin/ssh"
    # lists.gnu.org/archive/html/help-emacs-windows/2002-10/msg00109.html:
    export CYGWIN="binmode ntsec stty"	# I don't know what this does
    export winc="/cygdrive/c"
    export windows=1
#elif [ $uname == "windows32" ]; then #MinGW/MSYS
#elif [ $uname == "*MINGW*" ]; then #MinGW/MSYS # this syntax doesn't seem to work
elif [[ $uname == MINGW* ]]; then
    ls_args="${ls_args} --color"
    export windows=1
elif [ -d /dev/fs ]; then # SFU/SUA
    export winc="/dev/fs/C"
    export SVN_SSH="/usr/pkg/bin/ssh"
    test -f /usr/examples/win32/aliases.sh && /usr/examples/win32/aliases.sh
    export windows=1
elif [ $uname == "Darwin" ]; then # Mac OS X
    # many of these are thanks to <http://superuser.com/questions/52483/terminal-tips-and-tricks-for-mac-os-x>
    test -r /sw/bin/init.sh && source /sw/bin/init.sh  # fink
    function hideapp {
        APPLICATION="$1"
        osascript -e "tell application \"System Events\" to set visible of process \"$APPLICATION\" to false"
    }
    # Note that this works on X11 even when keyboard shortcuts are disabled in preferences :)
    alias switchx="osascript ~/doc/dhd/opt/ascript/x11-cmd-tab.ascript"
    # Launch QuickLook from the command line (^c will kill it and return to prompt)
    alias ql='qlmanage -p 2>/dev/null'
    function pman {
        man -t "${1}" | open -f -a /Applications/Preview.app
    }
    function pman2 {
        # doesn't ask if you want to save the manpage when you close the window in preview
        # on the other hand, i couldn't get it to work fuck it
        man -t "${1}" | ps2pdf - - | open -g -f -a /Applications/Preview.app 
    }
    # misc helpful commands: pbcopy/pbpaste, mdfind (-live for real time), afconvert, textutil
    # SetFile $file -a V # sets file invisible
elif [ $uname = "SunOS"  ]; then # Solaris
    export CC="/opt/csw/gcc4/bin/gcc"
l    psargs="-ef"
elif [ $uname == "OpenBSD" ]; then # obsd
    export PKG_PATH="ftp://ftp3.usa.openbsd.org/pub/OpenBSD/4.3/packages/sparc64/"
    psargs_user="j"
elif [ $uname == "Linux" ]; then # assume GNU userland
    if [ -d /usr/portage ]; then
        function ugrep {
            grep "$1" /usr/portage/profiles/use.desc \
                /usr/portage/profiles/use.local.desc
        }
        alias fnc 'find /etc -iname "._cfg????*"'
    fi
    ls_args="${ls_args} --color"
fi

if [ $winc ]; then # we are on Windows somehow
    # psh. I do all this and then realize that I should just use 
    # `runwin32 explorer` instead. 
    export windir="$winc/WINDOWS"
    export wins32="$winc/WINDOWS/system32"
    export progfiles="$winc/Program\ Files"
    export surf="$progfiles/Mozilla\ Firefox/firefox.exe"
    alias explorer="$windir/explorer.exe" # you *must* use \ paths and quote *everything*
    alias explore=explorer
    alias expl=explorer
    alias hh="$windir/hh.exe"
    alias regedit="$windir/regedit.exe"
    alias ifconfig="$wins32/ipconfig.exe"
    alias mstsc="$wins32/mstsc.exe"
    alias fsutil="$wins32/fsutil.exe"
    alias net="$wins32/net.exe"
    alias netsh="$wins32/netsh.exe"
fi

if [ $surf ]; then # we have a web browser!
    alias surf=$surf
    alias firefox=$surf
fi

# Neuric Aliases:
neuric="~/doc/neuric"
alias snu="svn update $neuric/{Neuric,Documentation,Training}"
alias sns="svn status $neuric/{Neuric,Documentation,Training} | grep -v '^\?'"
alias snsa="svn status $neuric/{Neuric,Documentation,Training}"
alias cdn="cd $neuric"

##################
# Global Aliases #
##################

alias ..="cd .."
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias wh="type -a" # under ksh you want wh=whence

alias sed='$cmd_sed'
alias du='$cmd_du'
alias dush='$cmd_du -sh'

alias ppath='echo $PATH | $cmd_sed "s/:/\n/g"'
alias pupath='echo $PATH | $cmd_sed "s/:/\n/g" | sort | uniq'
alias logrec='lsl /var/log | grep -v \\.bz2 | grep -v \\.0 | grep "`date +%b\ %d\ %k`"'

alias psa="ps $psargs"
alias psaj="ps $psargs$psargs_user"
alias psawcl="ps $psargs | wc -l"
function psasys {
    # if you are root, this will produce confusing results, since many system processes are of course run as root.
    psaj | grep -v "$me" 
}
alias psasyswcl="psasys | wc -l"
function psaf { 
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i $1 | grep -v "grep -i $1"
}
function define {
    wn "$1" -over
}
alias wno=define
function ff {
    find . -name "$1" -print
}

function bman {
    man $* | col -b | bcat
}


function smbputhelp {
cat <<EOF
This wrapper was designed because the syntax for smbclient is a huge piece of shit. Seriously. Who the hell decided to emulate the user friendliness of ftp for the love.

USAGE: 
You might use this script like so:
smbput file1 (file2...fileN) //server/share/path/to/somewhere
To see the available shares on a server, use:
smbclient -L //server

Samba includes its own smbget command, for which I will refer
the user to its own manual.
If you want to see smbclient's own silly syntax, here's some:
smbclient [-U username|-N] //server/share [password] <<DONEWITHTHAT
cd /path/to/somewhere
prompt
mput some files
exit
DONEWITHTHAT
Note that you should never put a trailing / after the share or
smbclient will be a giant fucking gay baby about it

FUTURE: 
- Grab all the things I need, and pass everything else on to smbclient.
  (That would certainly be the correct thing to do.) 
- Pass the -N argument to smbclient unless the user specifically 
  provides a username on the command line.
- Possibly support URL syntax instead of this shit
  (e.g. smb://[username[:password]@]server/share/path/)
- I have no idea how good my error handling is at present. (Probably
  awesome because I've put zero thought into it.) Investigate. 

EOF
}

function smbput {
    serverstring=${!#} # ${!#} is the final arg 
    # we are expecting a serverstring like //micah-pc/Users/Micah/Downloads
    server=`echo $serverstring | awk 'BEGIN {FS="/"}; {print $3}'`
    share=`echo  $serverstring | awk 'BEGIN {FS="/"}; {print $4}'`
    destination=`echo $serverstring | sed s/"\/\/$server\/$share\/"//`

    smbclient -U $user $server/$share $password <<EOF
prompt
cd $destination
mput $files
exit
EOF
}

function gpush {
    git commit $@
    git push
}

function .b {
    . ~/.profile
    . ~/.bashrc
}

function maildir2mbox {
    MDIR="$1"
    MBOX="$2"
    for msg in "$MDIR"/{cur,tmp,new}/*; do 
        formail -I Status: <"$msg" >> "$MBOX"
    done
}

# Important! this function assumes that you have no mbox files in already in your maildir!
# (That would be dumb of you, but I figured you could use fair warning anyway.)
# Also, I assume you're in the mbox root
function maildirtree2mboxes {
    maildir2mbox . Inbox.mbox
    # you have to do the exec call to 'bash -lc' because otherwise it won't find our
    # maildir2mbox function that we just so cleverly defined, argh
    find . -type d -depth 1 ! -path './tmp' ! -path './new' ! -path './cur' ! -path './courierimapkeywords' -exec bash -lc 'maildir2mbox "{}" "{}.mbox"' \;
    # if the file is hidden, unhide it
    for mboxfile in .*.mbox; do 
        mv "$mboxfile" "`basename \"$mboxfile\"|sed 's/^.//'`"
    done
}

# expects to be called like this: 
# maildirtree2mboxes-all-users /home .maildir
function maildirtree2mboxes-all-users {
    homedirs="$1"
    # the standard maildir name, usually something like Maildir or .mail
    maildirname="$2" 
    cd "$homedirs"
    for user in *; do cd "$user/$maildirname"; maildirtree2mboxes; cd ../..; done
}

# maybe this is too specific to script in here, but here's how i got the enron email
# leak into mbox files, one per folder:
    # mybasedir=`pwd` find . -type d -exec bash -c 'cd {}; for item in *; do if [ -f "$item" ]; then /usr/bin/formail -I Status: < "$item" >> "`basename {}`.mbox"; fi; done; cd "$mybasedir"' \;
# for the next step - renaming the mbox files to contain their relevant path info - 
# i started with this: 
    # find . -name \*mbox -exec mv {} `echo {}|sed -e 's#/#__#g'` \;
# but it just will never work because of the subshell. Ended up with this solution instead:
    # for mboxfile in $( find . -name \*.mbox ); do mv "$mboxfile" $( echo "$mboxfile" | sed s#./## | sed s#/#__#g ); done

alias ls="$cmd_ls $ls_args"
alias lsa="$cmd_ls $ls_args -a"
alias lsl="$cmd_ls $ls_args -al"
alias lsli="$cmd_ls $ls_args -ali" # lsl+inodes
alias l1="$cmd_ls $ls_args -1"
alias lslm="$cmd_ls $ls_args -lart" # lsl+ sort by modified time (lastest at bottom)
alias llm=lslm

function lsibash {
# sometimes (eg on Windows) you need to use hardlinks rather than symlinks
# to link your dotfiles from where they are in dhd/hbase/ into ~/
# this just lets you check to make sure they're still all the same inode
# i.e. the same file on disk. 
    ls -1i ~/.bashrc ~/.dhd/hbase/.bashrc
    ls -1i ~/.profile ~/.dhd/hbase/.profile
    ls -1i ~/.inputrc ~/.dhd/hbase/.inputrc
    ls -1i ~/.emacs ~/.dhd/hbase/.emacs
}

alias pu="pushd"
alias po="popd"
alias tailmes="tail -f /var/log/messages"
alias mess="less /var/log/messages"
alias dmesg="dmesg|less"
alias wcl="wc -l"

alias omg="echo wtf"
alias source=.

alias grep="$cmd_grep --color=auto"

# emacsy goodness
function e {
# note: emacsclient -n returns without waiting for you to kill the buffer in emacs
    macosxemacs="/Applications/Emacs/emacsformacosx.com/Emacs for Mac OS X.app"
    if [[ $uname == MINGW* ]]; then
        # try /c/opt/ntemacs24 first. then try the EmacsW32 possible locations.
        emacsdir="/c/opt/ntemacs24"
        if [ -d "/c/Program Files (x86)/Emacs/emacs" ]; then emacsdir="/c/Program Files (x86)/Emacs/emacs"
        elif [ -d "/c/Program Files/Emacs/emacs" ]; then emacsdir="/c/Program Files/Emacs/emacs"
        fi
        # note that patched emacs from EmacsW32 lets you run emacsclientw.exe whether you give it a filename or not
        # but that unpatched emacs requires you to run runemacs.exe first and then emacsclientw.exe subsequently
        if [ "$1" ]; then
            "${emacsdir}/bin/emacsclientw.exe" -n --alternate-editor="${emacsdir}/bin/runemacs.exe"
        else
            "${emacsdir}/bin/emacsclientw.exe" -n --alternate-editor="${emacsdir}/bin/runemacs.exe" "$1"
        fi
    elif [ -d "$macosxemacs"  ]; then
        if [ "$1" ]; then
            "$macosxemacs/Contents/MacOS/bin/emacsclient" -n "$1"
        else
            # check to see if there is an Emacs for Mac OS X.app process
            /usr/bin/open "$macosxemacs"
        fi
    elif [ -f /usr/local/bin/emacsclient ]; then
    # I believe this next bit requires Emacs23 under Unix: 
        if [ "$1" ]; then
            /usr/local/bin/emacsclient -a /usr/local/bin/emacs "$1"
        else 
            /usr/local/bin/emacsclient -a /usr/local/bin/emacs
        fi
    else 
        echo "Couldn't find emacs. Make sure it's installed and this function knows about it."
    fi
}

# screen stuff
cmd_screen=`type -P screen`
if [ $cmd_screen ]; then
    default_session_name="camelot" # this is used later on too
    
    # attach to session if extant, otherwise create a new one
    function scr {
        if [ $1 ]; then
            sessionname="$1"
        else
            sessionname="$default_session_name"
        fi
        $cmd_screen -D -R -S "$sessionname"
    }

    #alias screen="screen -D -R" 
    alias scrl="$cmd_screen -list"
    alias scrw="$cmd_screen -wipe"
fi

alias ssh="ssh -A"
# this way it won't save ssh host keys to ~/.ssh/known_hosts
alias sshtel="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scptel="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
function uploadid { 
    # this function could be extended to add the host to .ssh/config for use with my 'complete' line elsewhere in .bashrc
    cat ~/.ssh/rsa.bigger.key.pub | ssh $1 'cat - >> ~/.ssh/authorized_keys'
}


# Torrent &c stuff
function seedbox {
    for torrent in *.torrent; do
        scp "${torrent}" younix.us:~/.torincoming/
    done
    for nzb in *.nzb; do
        scp "${nzb}" younix.us:~/.nzbincoming/
    done
}
alias rmseed="rm *.torrent *.nzb"
alias lsseed="ls *.torrent *.nzb"
alias lseed=lsseed
alias rseed=rmseed

#alias nzb="hellanzb"
#alias nzbstart="hellanzb -D"
#alias nzbsite="hellanzb enqueuenewzbin"
#alias nzbfile="hellanzb enqueue"

function manualman {
# this is basically the function that man uses to view its manpages
# if you know the path to a manpage file (like /usr/share/man/man1/ls.1)
# you can view it directly with this function.
# this is particularly useful on MSYS for Windows because currently
# (20110413) there is a groff command but no man command for MSYS.
# (note that you will have to `mingw-get install msys-groff` first though)
    if [ `type -P groff` ]; then
        groff -Tascii -pet -mandoc -P-c "$1" | less -irs
    fi
}

# This is intended to be used in situations like album art scans, 
# where you have several image files that should be converted to
# one PDF so that iTunes can store it. 
# Note that you have to specify a filetype, not individual files. 
# It will convert all files of that filetype in the cwd. 
# Additionally, make sure there are no other PDFs files hanging
# around unless you want them assimilated into the final pdf too.
function convert2pdf {
    filetype="$1"
    oldpdfs=""
    for oldfile in *.$filetype; do 
        convert "$oldfile" "$oldfile.pdf"
        #oldpdfs="${oldpdfs} \"${oldfile}.pdf\""
    done
    pdftk *.pdf cat output Final.pdf
}

# Bulk replace file extensions on all files in current directory
# Use it like "changext html php" to move everything ending in .html to filename.php
function changext {
    oldext="$1"
    newext="$2"
    /bin/ls -1 *.$oldext | sed 's/\(.*\)\.$oldext/mv \"\1.$oldext\"  \"\1.$newext\"/' | /bin/sh
}


# GUI stuff
if [ $TERM_PROGRAM ]; then 
    function remote {
        if [ $2 ]; then 
            sessionname="$2"
        else 
            sessionname="$default_session_name"
        fi
        #osascript -e 'tell application "Terminal" to do script "ssh -t "$1" \"screen -D -R -S $sessionname\""'
        ssh -t "$1" "screen -D -R -S $sessionname"
    } 

    if   [ $TERM_PROGRAM == "Apple_Terminal" ]; then
        alias aterm='osascript -e "tell application \"Terminal\" to do script \"\""'
    elif [ $TERM_PROGRAM == "iTerm.app" ]; then
        alias iterm='osascript -e "tell application \"iTerm\" to do script \"\""'
        alias iterm='osascript -e "tell application \"iTerm\" \
            -e "activate"
            -e "set myterm to (make new terminal)"
            -e "tell myterm" 
                -e "set mysession to (make new session at the end of sessions)"
                -e "tell my session"
                    -e "exec command /bin/bash"
            -e "end tell" -e "end tell" -e "end tell"'

#osascript -e 'tell app "iTerm"' -e 'activate' -e 'set myterm to (make new terminal)' -e 'tell myterm' -e 'set mysession to (make new session at the end of sessions)' -e 'tell mysession' -e 'exec command "/bin/tcsh"' -e 'write text "INSERT SHELL COMMAND HERE"' -e 'end tell' -e 'end tell' -e 'end tell' 
    fi

#    term=
# This must come after the $TERM_PROGRAM check, because in Mac OS X, $DISPLAY is always
# set if you have X11 installed, since it opens X11 automagically if you open an X app. 
elif [ $DISPLAY ]; then
    term=xterm
    # if type -P urxvt >/dev/null; then # AKA rxvt-unicode
    #     term=urxvt
    # elif type -P rxvt >/dev/null; then
    #     term=rxvt
    # fi

    # intended to be used like `remote [user@]host [session]`
    # example: remote mrled@vlack.ath.cx rtorrent
    # sets the title and ssh's to first argument, and optionally  uses
    # the named screen session - if no such session exists, it uses my
    # default session name which is defined somewhere above
    # works for (u)rxvt and xterm at least
    function remote { 
        if [ $2 ]; then
            sessionname="$2"
        else 
            sessionname="$default_session_name"
        fi
        $term -T "$1" -e ssh -t "$1" "screen -D -R -S \"$sessionname\""
    }
    # you can check all sessions on a remote host with `ssh $host screen -ls`
fi

# Mac metadata files: .DS_Store and ._Doomsday.mkv for example
function mmf { 
    case $1 in 
        list) 
            find . -type f -name '._*'
            find . -type f -name '.DS_Store'
            ;;
        rm) 
            find . -type f -name '._*' -exec rm {} \;
            find . -type f -name '.DS_Store' -exec rm {} \;
            ;;
    esac
}

# Server files over http. This rules. 
# Serve all files under the directory this was run in. Does NOT serve an
# index page; you have to directly request the files themselves.
# Requires netcat as `nc`. 
# From <http://www.linuxscrew.com/2007/09/06/web-server-on-bash-in-one-line/>
function htserv {
    port=$1
    :;while [ $? -eq 0 ];do nc -vlp $port -c'(r=read;e=echo;$r a b c;z=$r;while [ ${#z} -gt 2 ];do $r z;done;f=`$e $b|sed 's/[^a-z0-9_.-]//gi'`;h="HTTP/1.0";o="$h 200 OK\r\n";c="Content";if [ -z $f ];then($e $o;ls|(while $r n;do if [ -f "$n" ]; then $e "`ls -gh $n`";fi;done););elif [ -f $f ];then $e "$o$c-Type: `file -ib $f`\n$c-Length: `stat -c%s $f`";$e;cat $f;else $e -e "$h 404 Not Found\n\n404\n";fi)';done
}

###################
# Other Functions #
###################
function listens {
    netstat -an | grep LISTEN | grep 'tcp|udp' | awk '{ print $1, "\t", $4 }' | sort
}
function connections {
    netstat -a | grep 'tcp|udp'
}
function sshbackdoor {
    username=`whoami`
    otherhost="-"
    port=1034
    wait=0
    while getopts "u:h:p:w:"; do
        case $opt in 
            u) 
                username=$OPTARG
                ;;
            h)
                otherhost=$OPTARG
                ;;
            p) 
                port=$OPTARG
                ;;
            w)
                if [[ $OPTARG != [0-9]* ]]; then
                    echo "The -w option must be an integer, in seconds, to wait. (Defaults to 0.)"
                    exit
                fi
                wait=$OPTARG
                ;;
            esac
        done
    if [ "$otherhost" -eq "-" ]; then 
        echo "You must at least provide a host with the -h option."
        exit
    fi
    sleep $wait && nohup ssh -f -N -R $port:localhost:22 $username@$otherhost
}
function routes {
    # works for macosx
    route -n get default
}

# LaTeX stuff:
function blix { #buildlatex
    latex "$1".tex
    dvipdf "$1".dvi "$1".pdf && rm "$1".dvi
    open "$1".pdf
}

function tname { 
    for f in $@; do 
        strings $f|head -n1|sed 's/.*name[1234567890]*://g' | sed 's/12:piece.*//g'
    done
}

function ttrac {
    for f in $@; do 
        strings $f|head -n1|sed 's/d8:announce[1234567890]*://g' | sed 's/10:creat.*//g'
    done
}

function thead {
    for f in $@; do 
        strings $f | head -n1
    done
}

function strip-comments {
    for f in $@; do
        grep -v '^#' $f | grep -v '^ *#' | grep -v '^$'
    done
}
# from http://www.robmeerman.co.uk/unix
# red stderr - prepend to a command to have its stderr output in red
function rse {
    # We need to wrap each phrase of the command in quotes to preserve arguments that contain whitespace
    # Execute the command, swap STDOUT and STDERR, colour STDOUT, swap back
    ((eval $(for phrase in "$@"; do echo -n "'$phrase' "; done)) 3>&1 1>&2 2>&3 | sed -e "s/^\(.*\)$/$(echo -en \\033)[31;1m\1$(echo -en \\033)[0m/") 3>&1 1>&2 2>&3
}


###################
# Global Settings #
###################

## Completion
complete -cf sudo
# SSH tab completion of hosts that exist in .ssh/config (via superuser.com)
if [ -f ~/.ssh/config ]; then
    complete -o default -o nospace -W "$(/usr/bin/env ruby -ne 'puts $_.split(/[,\s]+/)[1..-1].reject{|host| host.match(/\*|\?/)} if $_.match(/^\s*Host\s+/);' < $HOME/.ssh/config)" scp sftp ssh
fi


# glob filenames in a case-insensitive manner
# NOT the same as tab-complete case insensitively - you must add
#   set completion-ignore-case on
# in .inputrc for that.
shopt -s nocaseglob 
shopt -s checkwinsize

# set my editor to be correct
# (the -nw tells it not to open up a new window)
myeditor="emacs"
export EDITOR="$myeditor"
export VISUAL="$myeditor"
export FSEDIT="$myeditor"

# fucking CPAN
export PERL_MM_USE_DEFAULT=1

# last character of prompt
if [ $menum = 0 ]; then lcop='#'
else                    lcop='>'
fi

# Setting the default prompt
# Make sure that all of the non-printing characters in $PS1 are
# surrounded by \[ and \] - otherwise you will get retarded line
# wrapping problems. mmmmkay?
# Also, note that you can use \e instead of \033 in recent
# versions of bash. 
# 
#export PS1="\[\e[01;32m\]\u@\h\[\e[01;34m\] \w \$\[\e[00m\] "
#export PS1="\[\e[01;32m\]\u@\h\[\e[01;34m\] \w \$\[\e[00m\] "
# COLORS      bold,green           bold,blue         unbold,white
#           \[\e[01;32m\]     \[\e[01;34m\]      \[\e[00m\]
#      PS1="              \u@\h              \w \$           "
export PS1="\[\e[01;37m\]\t \[\e[01;34m\]\h\[\e[01;37m\]:\[\e[00;32m\]\W \[\e[01;34m\]$lcop \[\e[00m\]"
#                          \t              \h               :               \w              \$
# COLORS:    bold,white         normal,green      bold,blue       normal,white 
#export PS1="$ansi_bold $ansi_fg_white hello $ansi_fg_green sonny $ansi_fg_white $ansi_norm $ "
#export PS1="\t \w \$ "

unset lcop

