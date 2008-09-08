# .bashrc

export uname=`uname`
export host=`hostname`

## PATH... should come before everything else
# Future:
#  I should include a util that: 
#   - searches /etc /usr/etc /usr/local/etc /usr/nekoware/etc for execable files
#   - links them to /usr/etcbin or something like that
#   - man that is an abomination
# NOTE: this method does NOT seem to be workable if a directory has spaces

# don't include: /usr/ucb /usr/sfw/bin
# /opt/alternatives is where I put commands that I want to come BEFORE system-
# supplied commands in /bin. e.g. on Interix, su, make, ksh, and tar are all
# there, mostly as links to other commands such as /usr/pkg/bin/gtar. 
# This way, I don't break OS-updates by "updating" my version of /bin/tar
# with the new OS version of /bin/tar - /bin/tar always belongs to the OS, and 
# my stuff always belongs to me. 

PATH=
dirs=
dirs="${dirs} ~/opt/alternatives /opt/alternatives ~/opt/bin ~/opt/sbin"
dirs="${dirs} ~/doc/remote/dhd/hbase/local/bin ~/doc/remote/dhd/os/$uname/bin"
dirs="${dirs} /sw/bin /sw/sbin /opt/local/bin /opt/local/sbin"
dirs="${dirs} /usr/pkg/bin /usr/pkg/sbin"
dirs="${dirs} /usr/nekoware/bin /usr/nekoware/sbin /usr/freeware/bin"
dirs="${dirs} /opt/csw/bin /opt/csw/sbin /opt/csw/flex/bin /opt/csw/flex/sbin /opt/csw/gcc4/bin"
dirs="${dirs} /opt/csw/gcc4/sbin /opt/SUNWspro/bin /opt/SUNWspro/sbin"
dirs="${dirs} /opt/gcc.3.3/bin/i586-pc-interix3 /usr/local/MSVisualStudio/bin"
dirs="${dirs} /opt/gcc.3.3/bin /opt/ast/bin"
dirs="${dirs} /usr/contrib/bin /usr/contrib/win32/bin /usr/examples/admin"
dirs="${dirs} /mingw/bin /c/WINDOWS /c/WINDOWS/system32/Wbem /c/WINDOWS/system32 /c/opt/bin"
dirs="${dirs} /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin"
dirs="${dirs} /usr/games /usr/games/bin /usr/X11R6/bin /usr/X11R6/sbin /usr/bin/X11"

for p in ${dirs}; do
    if [ -d ${p} ]; then PATH="${PATH}${p}:"; fi
done
export PATH

umask 077 #stop reading my files!!
export CVS_RSH="ssh"

export cmd_ls="ls"
export cmd_sed="sed"
export cmd_du="du"

if [ $( type -P gls  ) ]; then cmd_ls=gls;   fi
if [ $( type -P gsed ) ]; then cmd_sed=gsed; fi
if [ $( type -P gdu  ) ]; then cmd_du=gdu;   fi

## Defaults which can be overridden in the system-specific configurations below
export psargs="ax"

if [ -d /cygdrive ]; then    # Cygwin
    # it inherits the Windows path, so if your Windows path has this set, 
    #  then it will be wrong:
    export SVN_SSH="/usr/bin/ssh"
    # lists.gnu.org/archive/html/help-emacs-windows/2002-10/msg00109.html:
    export CYGWIN="binmode ntsec stty"	# I don't know what this does
    export winc="/cygdrive/c"
    export windows=1
elif [ -d /dev/fs ]; then # SFU/SUA
    export winc="/dev/fs/C"
    export SVN_SSH="/usr/pkg/bin/ssh"
    
    test -f /usr/examples/win32/aliases.sh && /usr/examples/win32/aliases.sh
    export windows=1
elif [ $uname = "Darwin" ]; then # Mac OS X
    test -r /sw/bin/init.csh && source /sw/bin/init.csh  # fink
elif [ $uname = "SunOS"  ]; then # Solaris
    export CC="/opt/csw/gcc4/bin/gcc"
    export psargs="-ef"
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


##################
# Global Aliases #
##################

alias ..='cd ..'
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias wh=which

alias sed='$cmd_sed'
alias du='$cmd_du'

alias ppath='echo $PATH | $cmd_sed "s/:/\n/g"'
alias pupath='echo $PATH | $cmd_sed "s/:/\n/g" | sort | uniq'
alias logrec='lsl /var/log | grep -v \\.bz2 | grep -v \\.0 | grep "`date +%b\ %d\ %k`"'

alias psa="ps $psargs"
function psaf { 
    psa | grep -i $1 
}

alias ls='$cmd_ls -hF --color'
alias lsa='$cmd_ls -ahF --color'
alias lsl='$cmd_ls -alhF --color'
alias lsli='$cmd_ls -alhFi --color' # lsl+inodes
alias l1='$cmd_ls -1hF --color'
alias llm='$cmd_ls -lahrtF --color' # lists by last mod time

alias pu="pushd"
alias po="popd"
alias ff="find . -name '\!*' -print"
alias tailmes='tail -f /var/log/messages'
alias mess="less /var/log/messages"
alias dmesg='dmesg|less'
alias define='wn \!* -over'
alias .b=". ~/.bashrc"
alias listen='netstat -a | grep LISTEN'

alias omg='echo wtf'
alias source=.
alias .b='. ~/.bashrc'

###################
# Global Settings #
###################

# set my editor to be correct
# (the -nw tells it not to open up a new window)
export myeditor="emacs"
export EDITOR="$myeditor"
export VISUAL="$myeditor"
export FSEDIT="$myeditor"
unset myeditor

bind '"\ep": history-search-backward'
bind '"\en": history-search-forward'

# Setting the default prompt
export PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "

