# .bashrc

## Set the path to inclode only directories that exist on this system.
#   - not workable if the directory has spaces (!!)
#   - put commands that should come before system commands in {,~}/opt/alternatives/

h="${HOME}"
PATH=
d=
d="${d} $h/.rvm/bin"
d="${d} $h/opt/bin $h/opt/sbin $h/.dhd/opt/bin"
d="${d} $h/opt/homebrew/bin $h/opt/homebrew/sbin"
d="${d} /opt/homebrew/bin /opt/homebrew/sbin"
#d="${d} /usr/local/homebrew/bin /usr/local/homebrew/sbin"
#d="${d} /sw/bin /sw/sbin /opt/local/bin /opt/local/sbin /Developer/usr/bin /Developer/usr/sbin"
d="${d} /usr/pkg/bin /usr/pkg/sbin"
d="${d} /usr/local/bin /usr/local/sbin /usr/bin /usr/sbin /bin /sbin"
d="${d} /c/WINDOWS /c/WINDOWS/system32/Wbem /c/WINDOWS/system32"
d="${d} /c/MinGW/bin /c/MinGW/sbin /c/MinGW/msys/1.0/bin /c/MinGW/msys/1.0/sbin"

for p in ${d}; do
    if [ -d ${p} ]; then PATH="${PATH}${p}:"; fi
done
export PATH

# Homebrew library bullshit
# This doesn't seem to work??
# hbdir="${HOME}/opt/homebrew"
# if ! echo $LIBRARY_PATH | grep "$hbdir/include" >/dev/null; then
#     export LIBRARY_PATH="${LIBRARY_PATH}:${hbdir}/include"
# fi
# if ! echo $DYLD_LIBRARY_PATH | grep "$hbdir/lib" >/dev/null; then
#     export DYLD_LIBRARY_PATH="${DYLD_LIBRARY_PATH}:${hbdir}/lib"
# fi
# export LDFLAGS='-L$hbdir/include'


#if [ -d $HOME/opt/homebrew ]; then 
    #export CFLAGS="-I$HOME/opt/homebrew/include -L$HOME/opt/homebrew/lib"
    #export CPPFLAGS="-I$HOME/opt/homebrew/include -L$HOME/opt/homebrew/lib"
    #export LDFLAGS='-L$HOME/opt/homebrew/lib -Wl,-rpath $HOME/opt/homebrew/lib'
#    export LDFLAGS=-L$HOME/opt/homebrew/lib
#    export CPPFLAGS=-I$HOME/opt/homebrew/include
#fi


unset d h


# Ruby RVM bullshit
# install with `curl -L https://get.rvm.io | bash -s stable --ruby`
#if [[ -s "/usr/local/rvm/scripts/rvm" ]]; then
#fi
# this is actually not necessary for rvm WHOA WOW WHOA
#rvm() { sudo -H sh -c "umask 022; rvm $*"; }

# I think I can replace this with $OSTYPE but I'll need to test it on all the different OSes I have below
uname=`uname`

umask 077
export CVS_RSH="ssh"

cmd_ls="ls"
ls_args="-hF --color"
cmd_sed="sed"
cmd_du="du"
cmd_grep="grep"

if type -P gsed >/dev/null; then cmd_sed=gsed; fi
if type -P gdu  >/dev/null; then cmd_du=gdu;   fi
if type -P ack-grep >/dev/null; then alias ack=ack-grep; fi

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
#elif [[ $uname == MINGW* ]]; then
elif [[ $OSTYPE == mingw ]]; then
    ls_args="${ls_args} --color"
#elif [[ $uname == FreeBSD ]]; then
elif [[ $OSTYPE == freebsd* ]]; then
    ls_args="-hFG"
elif [ -d /dev/fs ]; then # SFU/SUA
    export winc="/dev/fs/C"
    export SVN_SSH="/usr/pkg/bin/ssh"
    test -f /usr/examples/win32/aliases.sh && /usr/examples/win32/aliases.sh
elif [ $uname == "Darwin" ]; then # Mac OS X
    ls_args="-hFG"
    if type -P gls >/dev/null; then 
        cmd_ls=gls
        ls_args="-hF --color"
    fi
    # many of these are thanks to <http://superuser.com/questions/52483/terminal-tips-and-tricks-for-mac-os-x>
    test -r /sw/bin/init.sh && source /sw/bin/init.sh  # fink
    # Note that this works on X11 even when keyboard shortcuts are disabled in preferences :)
    alias switchx="osascript ~/.dhd/opt/ascript/x11-cmd-tab.ascript"
    # Launch QuickLook from the command line (^c will kill it and return to prompt)
    alias ql='qlmanage -p 2>/dev/null'
    pman() {
        man -t $* | ps2pdf - - | open -g -f -a /Applications/Preview.app 
    }
fi

# acutally, might want to use `see` instead, hmm. 
# oh geez there is also `gnome-open`.
# also consider using a gconf errors file, like ~/.fucking-stupid-gconf-bullshit
# and exporting that from .bashrc so that other scripts can use it too #ballin #sorry
if [ $uname != "Darwin" ]; then
    if type -p xdg-open 2>&1 > /dev/null ; then
        alias open=xdg-open
    fi
fi

#######################
# Host-specific stuff #
#######################
if [[ $HOST == "selene" ]]; then
    alias anonymize="sudo -H -u t"
fi

#if [[ $HOST == "anyanka" ]]; then
#    export USE_CCACHE=1
#fi
# suggested ccache size is 50-100gb fuck

##################
# Global Aliases #
##################

alias ..="cd .."
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias zl=zless
# the following tested initially on Mac OS X, less v 418
LESS="-icdM"
alias wh="type -a" # under ksh you want wh=whence

alias sed='$cmd_sed'
alias du='$cmd_du'
alias dush='$cmd_du -sh'

# print the path, one item per line
alias ppath='echo $PATH | $cmd_sed "s/:/\n/g"' 
alias logrec='lsl /var/log | grep -v \\.bz2 | grep -v \\.0 | grep "`date +%b\ %d\ %k`"'

alias psa="ps $psargs"
alias psaj="ps $psargs$psargs_user"
alias psawcl="ps $psargs | wc -l"
psother() {
    # return all processes except my own
    psaj | grep -v "$USER" 
}
psaf() { 
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i $1 | grep -v "grep -i $1"
}
define() {
    wn "$1" -over
}
alias wno=define
ff() {
    find . -name "$1" -print
}

rebash() {
    . ~/.profile
    . ~/.bashrc
}
alias .b=rebash

maildir2mbox() {
    MDIR="$1"
    MBOX="$2"
    for msg in "$MDIR"/{cur,tmp,new}/*; do 
        formail -I Status: <"$msg" >> "$MBOX"
    done
}

# Important! this function assumes that you have no mbox files in already in your maildir!
# (That would be dumb of you, but I figured you could use fair warning anyway.)
# Also, I assume you're in the mbox root
maildirtree2mboxes() {
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
# maildirtree2mboxes_all_users /home .maildir
maildirtree2mboxes_all_users() {
    homedirs="$1"
    # the standard maildir name, usually something like Maildir or .mail
    maildirname="$2" 
    cd "$homedirs"
    for user in *; do cd "$user/$maildirname"; maildirtree2mboxes; cd ../..; done
}

findunreadable() {
    for path in $*; do
        # find all of $path, print each found file to /dev/null, and then redirect stderr to stdout
        # the final redirection lets us pipe this command into e.g. wc -l
        find "$path" -fprint /dev/null 2>&1
    done
}

alias ls="$cmd_ls $ls_args"
alias lsa="$cmd_ls $ls_args -a"
alias lsl="$cmd_ls $ls_args -al"
alias lsli="$cmd_ls $ls_args -ali" # lsl+inodes
alias l1="$cmd_ls $ls_args -1"
alias lslm="$cmd_ls $ls_args -lart" # lsl+ sort by modified time (lastest at bottom)
alias llm=lslm

alias pu="pushd"
alias po="popd"
alias tailmes="tail -f /var/log/messages"
alias mess="less /var/log/messages"
alias dmesg="dmesg|less"
alias wcl="wc -l"

alias omg="echo wtf"
alias source=.
alias shl="echo $SHLVL"

alias grep="$cmd_grep --color=auto"

if type -P xscreensaver-command >/dev/null; then alias xslock="xscreensaver-command -lock"; fi

disp() {
    if [ -z $1 ]; then
        echo "DISPLAY=$DISPLAY"
    elif [ "$1" = "-h" ]; then
        echo "disp - set the $DISPLAY variable, simply"
        echo "    Usage: 'disp [N]', where N is an integer"
        echo "Sets the display environment variable to ':N'."
        echo "If N was not passed, it sets it to ':0' instead."
        echo "TODO: expand this to work better with reattached screen sessions and ssh."
    elif [[ $1 =~ ^[0-9]+$ ]]; then # if $1 is an integer
        echo Setting DISPLAY to ":$1"
        export DISPLAY=":$1"
    else
        echo "The first argument must be an integer; see 'disp -h' for more details."
    fi
}
xttitle() {
    echo -e "\033]2;""$1""\007"
}
alias xtt=xtermtitle

alias ddate="date +%Y%m%d"
epoch() {
    if [ -z "$1" ]; then d="now"
    else d="$@"
    fi

    if (date --date="$d" &>/dev/null); then
        tunix=`date --date="$d" +%s`
        thuman=`date --date="$d"`
        echo "Unix time: $tunix    Date: $thuman    Input: $d"
    else
        echo "Invalid date: $d"
    fi
}

# emacsy goodness
e() {
# note: emacsclient -n returns without waiting for you to kill the buffer in emacs
    macosxemacs="/Applications/Emacs.app"
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
    elif `type -P emacsclient >/dev/null`; then
        emacsclient -n $@
    else 
        echo "Couldn't find emacs. Make sure it's installed and this function knows about it."
    fi
}

# screen stuff
cmd_screen=`type -P screen`
default_session_name="camelot" # totally arbitrary session name; note that it IS used elsewhere, though, such as .xsession-stumpwm, where I have it launch an xterm that connects to this session

# Creates an Xterm window title of user@host <screen session name>, but only if running inside screen
if [ $STY ]; then
    # $STY looks like 123123.camelot; just grab the text name and ignore the number:
    session_name=${STY#*.} 
    screen_window_hardstatus="${USER}@${HOSTNAME} <${session_name}>"
    echo -ne "\033]2;${screen_window_hardstatus}\007"
fi

# attach to session if it exists, otherwise create a new one
scr() {
    # First: grab out named arguments like -r
    # After that: grab positional arguments like $1, which will be the session name if it is present
    # Note that you can specify -r <host> <session name>, and it will process the host fiest
    #   and still see <session name> as $1. 
    i=0; pctr=0; argcount=$#; declare -a posargs; remote=false

    # These functions are redefined if debug mode is turned on
    #execute the argumentsd directory (normal mode; will change this in debug mode)
    ee(){ $*; }
    #noop; totally ignore arguments.
    debugprint() { false; }

    scr_help() {
        echo "scr() [-h|--help] [-r|--remote <REMOTE HOST>] [-d|--debug] "
        echo "      [SESSION NAME] [-- <SSH ARGUMENTS>]"
        echo "Screen session management wrapper thing."
        echo "    -r <REMOTE HOST>: connect to a screen session on a remote host."
        echo "    -d: Print debug messages (probably useless)."
        echo "    -h: Print help and exit."
        echo "    SESSION NAME: provide an optional session name. Default is 'camelot'."
        echo "        It is recommended to use the default until you need more than one"
        echo "        session on a given host."
        echo "    --: Indicates that all remaining arguments should be passed to ssh."
        echo "        For example: scr -r example.com -- -i ~/.ssh/special_id_rsa"
    }
    while [ $i -lt $argcount ]; do
        case "$1" in
            -r | --remote )
                # increment i twice because we are eating 2 arguments
                remote=true; rhost=$2; ((i+=2)); shift 2;;
            -d | --debug )
                # don't execute, just print
                ee() { echo $*; }
                # print debug statements too
                debugprint() { echo $*; }
                debugprint "Debuggin'"
                shift;; 
            -h | --help )
                scr_help
                return
                ;;
            --)
                declare -a sa
                sctr=0
                shift
                while [ $i -lt $argcount ]; do
                    sa[$sctr]=$1
                    ((sctr++))
                    ((i++))
                    shift
                done
                ;;
            *)
                # if the first character of $1 is a '-', give an error
                # for the syntax see e.g.: http://www.softpanorama.org/Scripting/Shellorama/Reference/string_operations_in_shell.shtml
                [ $1 ] && if [ ${1:0:1} == "-" ]; then 
                    echo "Error: you supplied option '$1', but there is no such option"
                    scr_help
                    return
                fi
                #posargs = positional args
                posargs[pctr]=$1; ((pctr++)); ((i++)); 
                shift;;
        esac
    done

    if [ $pctr -gt 1 ]; then
        echo "Error: you supplied too many positional arguments"
        scr_help
        return
    elif [ $posargs ]; then
        sessionname=$posargs #posargs will never have more than 1 so this is safe in this function
    else
        sessionname="${default_session_name}"
    fi
    debugprint "Session name: $sessionname"

    # if you're in a screen session and creating a new one, use a different escape key (handy)
    if [[ $TERM == "screen" ]]; then
        # -e :: changes the screen escape key. NOT set in .screenrc! otherwise that *sometimes* overrides cli option (bug?)
        scrargs="-e^]]"
        debugprint "Running inside a screen session, going to use srcargs: ${srcargs}"
    else
        scrargs="-e^tt"
        debugprint "Not running inside screen, going to use srcargs: ${srcargs}"
    fi

    sshargs=" "
    if $sa; then
        i=0
        while [ $i -lt ${#sa} ]; do
            sshargs+=" ${sshargs[$i]}"
            ((i++))
        done
    fi

    screen_call="screen $scrargs -D -R -S $sessionname"
    if $remote; then
        ee ssh $sshargs -t $rhost "$screen_call"
    else
        ee $screen_call
    fi
}
alias scrl="$cmd_screen -list"
alias scrw="$cmd_screen -wipe"
remote() {
    # I should really call scr -r instead, but just in case I forget
    scr -r $1 $2
} 

##
## Remote Commands
##

# this way it won't save ssh host keys to ~/.ssh/known_hosts
alias sshtel="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scptel="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
uploadid() { 
    # this function could be extended to add the host to .ssh/config for use with my 'complete' line elsewhere in .bashrc
    cat ~/.ssh/id_rsa.pub | ssh $* 'mkdir -p ~/.ssh && cat - >> ~/.ssh/authorized_keys'
}
alias ssh-uploadid="uploadid"
_fingerprint='
    # it is usually in /etc/ssh
    if ls /etc/ssh/ssh_host*key.pub >/dev/null 2>&1; then
        for publickey in /etc/ssh/ssh_host*key.pub; do 
            ssh-keygen -lf "$publickey"
        done
    # but on the mac the ssh configuration is just directly in /etc
    elif ls /etc/ssh_host*key.pub >/dev/null 2>&1; then
        for publickey in /etc/ssh_host*key.pub; do
            ssh-keygen -lf "$publickey"
        done
    fi
'
fingerprint() {
    echo "Local key(s):"
    echo "$_fingerprint" | /bin/bash

    for host in $@; do
        echo "Remote key(s) on $host:"
        echo "$_fingerprint" | ssh -Tq $host
    done
}

# wake-on-lan information so I don't have to always remember it
magicp() { 
    target=${1:? "Usage: magicp <target>, where <target> is a host that I know about"}
    if   [[ $target == "andraia-wifi" ]]; then
        wakeonlan 00:1f:f3:d8:40:e6 
    elif [[ $target == "andraia-wired" ]]; then
        wakeonlan 00:1f:5b:ca:32:40 
    elif [[ $target == "andraia" ]]; then
        wakeonlan 00:1f:f3:d8:40:e6         
        wakeonlan 00:1f:5b:ca:32:40 
    else
        echo "I don't know about host \"$target\" yet"
        echo "You can use \`wakeonlan <macaddr>\` to wake it up."
    fi
}

esxtop() {
    ssh root@antimony.djinn.internal -t TERM=xterm esxtop
}

alias canhazip='curl icanhazip.com'
alias whatismyip=canhazip
alias icanhazip=canhazip
# gets public ip via dns. can help for when behind pay hotspots. 
# via https://twitter.com/climagic/status/220977468360765442 / @brimston3
# some hotspots fuck with udp/53; you might try dig +tcp if that happens
alias dnsip='dig myip.opendns.com  @resolver1.opendns.com +short' 

alias truecrypt="/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
alias hping=hping3
alias hp=hping3

# Torrent &c stuff
seedbox() {
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

webstream() {
    # take N video URLs on the cli and stream them with mplayer. Works with any site youtube-dl can see. 
    for video in $*; do
        youtube-dl -q -o- "$video" | mplayer -really-quiet -cache 1000 -
    done
}
mencinfo() {
    for vidfile in "$@"; do
        mplayer -identify "$vidfile" -ao null -vo null -frames 0 2>/dev/null | while read line; do
            if [ -z "$line" ]; then 
                :
            else
                echo -en "\033[35m$vidfile\033[0m::  "
                echo -e "$line"
            fi
        done
    done
}
ffinfo() {
    for vidfile in "$@"; do 
        #echo -e "\033[35m$vidfile\033[0m"
        # ffmpeg prints a giant useless header; let's not output it 
        in_header=1
        ffmpeg -i "$vidfile" 2>&1 | while read line; do
            if [[ "$line" == Input* ]]; then
                in_header=0
            elif [[ "$line" == At\ least\ one\ output\ file* ]]; then
                in_header=1
            fi
            if [ "$in_header" -eq 0 ]; then
                #echo -en "\t"
                echo -en "\033[35m$vidfile\033[0m::  "
                echo -e "$line"
            fi
        done
    done
}

extractaudio() {
    for v in "$@"; do
        ffmpeg -i "$v" -acodec copy -vn "$v.m4a"
    done
    echo "The files are written as .m4a files but we didn't check first!"
    echo "(You can check yourself with 'ffmpeg -i "vidfile" 2>&1|grep Audio'"
}

manualman() {
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
convert2pdf() {
    filetype="$1"
    for oldfile in *.$filetype; do 
        convert "$oldfile" "$oldfile.pdf"
    done
    pdftk *.pdf cat output Final.pdf
}

# Bulk replace file extensions on all files in current directory
# Use it like "changext html php" to move everything ending in .html to filename.php
changext() {
    oldext="$1"
    newext="$2"
    /bin/ls -1 *.$oldext | sed 's/\(.*\)\.$oldext/mv \"\1.$oldext\"  \"\1.$newext\"/' | /bin/sh
}

# Mac metadata files: .DS_Store and ._Doomsday.mkv for example
mmf() { 
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

# fucking quarantine thing
# could also turn it off completely: 
# defaults write com.apple.LaunchServices LSQuarantine -bool false
unquarantine() { 
    for f in $@; do
        xattr -r -d com.apple.quarantine $f
    done
}

# via: http://stackoverflow.com/questions/296536/urlencode-from-a-bash-script
# THIS IS BROKEN if it encounters a space it just stops processing altogether ugh.
rawurlencode() {
  local string="${1}"
  local strlen=${#string}
  local encoded=""

  for (( pos=0 ; pos<strlen ; pos++ )); do
     c=${string:$pos:1}
     case "$c" in
        [-_.~a-zA-Z0-9] ) o="${c}" ;;
        * )               printf -v o '%%%02x' "'$c"
     esac
     encoded+="${o}"
  done
  echo "${encoded}"    # You can either set a return variable (FASTER) 
  REPLY="${encoded}"   #+or echo the result (EASIER)... or both... :p
}

# Returns a string in which the sequences with percent (%) signs followed by
# two hex digits have been replaced with literal characters.
rawurldecode() {

  # This is perhaps a risky gambit, but since all escape characters must be
  # encoded, we can replace %NN with \xNN and pass the lot to printf -b, which
  # will decode hex for us

  printf -v REPLY '%b' "${1//%/\\x}" # You can either set a return variable (FASTER)

  echo "${REPLY}"  #+or echo the result (EASIER)... or both... :p
}

    

# Serve files over http. This rules. 
# Serve all files under the directory this was run in. Does NOT serve an
# index page; you have to directly request the files themselves.
# Requires netcat as 'nc'. 
# From <http://www.linuxscrew.com/2007/09/06/web-server-on-bash-in-one-line/>
htserv() {
    port=$1
#    :;while [ $? -eq 0 ];do nc -vlp $port -c'(r=read;e=echo;$r a b c;z=$r;while [ ${#z} -gt 2 ];do $r z;done;f=`$e $b|sed 's/[^a-z0-9_.-]//gi'`;h="HTTP/1.0";o="$h 200 OK\r\n";c="Content";if [ -z $f ];then($e $o;ls|(while $r n;do if [ -f "$n" ]; then $e "`ls -gh $n`";fi;done););elif [ -f $f ];then $e "$o$c-Type: `file -ib $f`\n$c-Length: `stat -c%s $f`";$e;cat $f;else $e -e "$h 404 Not Found\n\n404\n";fi)';done
    :;while [ $? -eq 0 ];do nc.traditional -vlp $port -c'(r=read;e=echo;$r a b c;z=$r;while [ ${#z} -gt 2 ];do $r z;done;f=`$e $b|sed 's/[^a-z0-9_.-]//gi'`;h="HTTP/1.0";o="$h 200 OK\r\n";c="Content";if [ -z $f ];then($e $o;ls|(while $r n;do if [ -f "$n" ]; then $e "`ls -gh $n`";fi;done););elif [ -f $f ];then $e "$o$c-Type: `file -ib $f`\n$c-Length: `stat -c%s $f`";$e;cat $f;else $e -e "$h 404 Not Found\n\n404\n";fi)';done
}

###################
# Other Functions #
###################
bash_listens() {
    netstat -an | egrep '((tcp)|(udp)).*LISTEN' | awk '{ print $1, "\t", $4 }' | sort
}
lsof_listens() {
    lsof +M -iTCP -sTCP:LISTEN
    lsof +M -iUDP
}

routes() {
    # works for macosx
    route -n get default
    # netstat -r would work for others
}

tinfo() { # bittorrent info
    case $1 in
        name)
            for f in $@; do 
                strings $f|head -n1|sed 's/.*name[1234567890]*://g' | sed 's/12:piece.*//g'
            done
            ;;
        trac)
            for f in $@; do 
                strings $f|head -n1|sed 's/d8:announce[1234567890]*://g' | sed 's/10:creat.*//g'
            done
            ;;
        head)
            for f in $@; do 
                strings $f | head -n1
            done
            ;;
    esac
}

strip_comments() { 
    for f in $@; do
        grep -v '^[	| ]*#'  $f | grep -v '^[	| ]*$' 
        #grep -v '^[:blank:]*#' $f | grep -v '^[:blank:]*$'
    done
}
# from http://www.robmeerman.co.uk/unix
# red stderr - prepend to a command to have its stderr output in red
rse() {
    # We need to wrap each phrase of the command in quotes to preserve arguments that contain whitespace
    # Execute the command, swap STDOUT and STDERR, colour STDOUT, swap back
    ((eval $(for phrase in "$@"; do echo -n "'$phrase' "; done)) 3>&1 1>&2 2>&3 | sed -e "s/^\(.*\)$/$(echo -en \\033)[31;1m\1$(echo -en \\033)[0m/") 3>&1 1>&2 2>&3
}

sprunge() {
    curl -F 'sprunge=<-' http://sprunge.us
}

matrix() { # shows matrix code. via @climagic
    TR_CMD=tr
    type -P gtr > /dev/null && TR_CMD=gtr
    while [ 1 ]; do dd if=/dev/urandom bs=$(($COLUMNS * 3)) count=1 2> /dev/null| $TR_CMD -c "[:digit:]" " " | GREP_COLOR="1;32" grep --color "[^ ]"; sleep 0.3; done
}
# silly progress spinner from @climagic
roll () { for t in {1..20} ; do for i in '|' / - '\' ; do echo -ne "\b\b $i" ; sleep 0.1 ; done ; done ; echo ;} 

export PYTHONSTARTUP=~/.dhd/hbase/python.profile
#export IPYTHONDIR=~/.dhd/hbase/ipython
# fuck python2
if type -P ipython3  >/dev/null; then
    alias ipy=ipython3
    alias ipython=ipython3
fi
export PYTHONPATH=${HOME}/.dhd/opt/python

###################
# Global Settings #
###################

## Completion
complete -cf sudo
# via <http://hints.macworld.com/article.php?story=20080317085050719>
_complete_ssh_hosts() {
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    sshkh=
    sshcnf=
    comp_ssh_hosts=`
        if [ -f ~/.ssh/known_hosts ]; then sed -e 's/^  *//' -e '/^$/d' -e 's/[, ].*//' -e '/\[/d' ~/.ssh/known_hosts | sort -u; fi
        if [ -f ~/.ssh/config ]; then grep '^[Hh]ost ' ~/.ssh/config | awk '{print $2}'; fi
        `
    COMPREPLY=( $(compgen -W "${comp_ssh_hosts}" -- $cur))
    return 0
}
complete -F _complete_ssh_hosts ssh

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

# fucking Perl/CPAN
export PERL_MM_USE_DEFAULT=1

# fucking umask issues on all these fucking tools, fuck you guys, fyuckfuyckakiguyh

#cpan()         { sudo -H sh -c "umask 022; cpan $*";  }
#pip()          { sudo -H sh -c "umask 022; pip $*"; }
#pip3()         { sudo -H sh -c "umask 022; pip3 $*"; }
#easy_install() { sudo -H sh -c "umask 022; easy_install $*"; }

# last character of prompt
if   [ $UID = 0 ]; then #root user
    lcop='#'
elif [ $USER = "t" ]; then  #tor user
    lcop='?'
else                      #normal user
    lcop='>'
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
#                        \t              \h             :             \W              >
# COLORS:    bold,white         normal,green      bold,blue       normal,white 
#export PS1="$ansi_bold $ansi_fg_white hello $ansi_fg_green sonny $ansi_fg_white $ansi_norm $ "
#export PS1="\t \w \$ "

unset lcop

