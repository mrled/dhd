# .bashrc

## Set the path to inclode only directories that exist on this system.
#   - not workable if the directory has spaces (!!)
#   - put commands that should come before system commands in {,~}/opt/alternatives/
h="${HOME}"
PATH=
d=
d="${d}/Library/Frameworks/Python.framework/Versions/2.7/bin/python"
d="${d} /usr/local/texlive/2008/bin/universal-darwin"
d="${d} $h/opt/alternatives /opt/alternatives $h/opt/bin $h/opt/sbin"
d="${d} $h/.dhd/opt/bin"
# fuck you Homebrew, installing to /usr/local is bullshit
d="${d} $h/opt/homebrew/bin $h/opt/homebrew/sbin $h/opt/homebrew/Cellar/ruby/1.9.3-p0/bin"
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
d="${d} /c/opt/svn/bin /c/opt/SysinternalsSuite /c/opt/nirsoft_package/NirSoft /c/opt/nirsoft64"
# BE CAREFUL: if your C:\opt contains ls and friends from UnxUtils or GnuWin32, 
# you might not want to add it here
d="${d} /c/opt/bin /c/opt/sbin /c/opt/local/bin /c/opt/local/sbin"
# this should go last becausae it has some things that won't work with MinTTY like vim and sh.exe
d="${d} /c/opt/git/bin"

for p in ${d}; do
    if [ -d ${p} ]; then PATH="${PATH}${p}:"; fi
done
export PATH
unset d h

uname=`uname`
host=`hostname`
me=`whoami`
menum=`id -u`

umask 077
export CVS_RSH="ssh"

cmd_ls="ls"
ls_args="-hF --color"
cmd_sed="sed"
cmd_du="du"
cmd_grep="grep"

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
elif [[ $uname == MINGW* ]]; then
    ls_args="${ls_args} --color"
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

#######################
# Host-specific stuff #
#######################
if [ $host == "selene" ]; then
    alias anonymize="sudo -H -u t"
fi


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
    psaj | grep -v "$me" 
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
    macosxemacs="/Applications/Emacs for Mac OS X.app"
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
if [ $cmd_screen ]; then
    default_session_name="camelot" # totally arbitrary session name; note that it IS used elsewhere, though, such as .xsession-stumpwm, where I have it launch an xterm that connects to this session
    
    # attach to session if extant, otherwise create a new one
    scr() {
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

##
## Remote Commands
##
alias ssh="ssh -A"
# this way it won't save ssh host keys to ~/.ssh/known_hosts
alias sshtel="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scptel="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
uploadid() { 
    # this function could be extended to add the host to .ssh/config for use with my 'complete' line elsewhere in .bashrc
    cat ~/.ssh/id_rsa.pub | ssh $* 'mkdir -p ~/.ssh && cat - >> ~/.ssh/authorized_keys'
}
alias ssh-uploadid="uploadid"
fingerprint() {
    for publickey in /etc/ssh/*.pub; do 
        echo "Local key: " `ssh-keygen -lf "$publickey"`
    done
    for argument in $@; do
        echo "$argument key: " `ssh $argument 'for publickey in /etc/ssh/*.pub; do ssh-keygen -lf $publickey; done'`
    done
}
alias ssh-fingerprint="fingerprint"
rfingerprint() {
    for argument in $@; do
        echo "SSH keys for $argument"
        ssh $argument 'for publickey in /etc/ssh/*.pub; do ssh-keygen -lf $publickey; done'
    done
}
alias ssh-rfingerprint="rfingerprint"

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

remote() {
    if [ $2 ]; then 
        sessionname="$2"
    else 
        sessionname="$default_session_name"
    fi
    ssh -t "$1" "screen -D -R -S $sessionname"
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

# Serve files over http. This rules. 
# Serve all files under the directory this was run in. Does NOT serve an
# index page; you have to directly request the files themselves.
# Requires netcat as 'nc'. 
# From <http://www.linuxscrew.com/2007/09/06/web-server-on-bash-in-one-line/>
htserv() {
    port=$1
    :;while [ $? -eq 0 ];do nc -vlp $port -c'(r=read;e=echo;$r a b c;z=$r;while [ ${#z} -gt 2 ];do $r z;done;f=`$e $b|sed 's/[^a-z0-9_.-]//gi'`;h="HTTP/1.0";o="$h 200 OK\r\n";c="Content";if [ -z $f ];then($e $o;ls|(while $r n;do if [ -f "$n" ]; then $e "`ls -gh $n`";fi;done););elif [ -f $f ];then $e "$o$c-Type: `file -ib $f`\n$c-Length: `stat -c%s $f`";$e;cat $f;else $e -e "$h 404 Not Found\n\n404\n";fi)';done
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
if [ -x `type -p ikiwiki` ]; then alias iw=`type -p ikiwiki`; fi

# last character of prompt
if   [ $menum = 0 ]; then #root user
    lcop='#'
elif [ $me = "t" ]; then  #tor user
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

