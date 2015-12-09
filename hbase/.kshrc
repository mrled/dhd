# .profile for **KSH**

uname=`uname`
host=`hostname`
me=`whoami`

## Set the path
#   - not workable if the directory has spaces
#   - put commands that should come before system commands in {,~}/opt/alternatives/
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


#??export HOME TERM 
umask 077 #stop reading my files!!
export CVS_RSH="ssh"

cmd_ls="ls"
ls_args="-hF"
cmd_sed="sed"
cmd_du="du"
#export cmd_ls cmd_sed cmd_du # do I need to do this?

if   [ $( whence gls ) ];     then cmd_ls=gls;   
elif [ $( whence colorls ) ]; then cmd_ls=colorls;
                                   ls_args="${ls_args} -G"; fi
if [ $( whence gsed ) ]; then cmd_sed=gsed; fi
if [ $( whence gdu  ) ]; then cmd_du=gdu;   fi

## Defaults which can be overridden in the system-specific configurations below
export psargs="ax"
export psargs_user="j"

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
elif [ $uname = "OpenBSD" ]; then # obsd
    export PKG_PATH=ftp://ftp.openbsd.org/pub/OpenBSD/4.3/packages/sparc64/
    psargs_user="j"
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

alias ..="cd .."
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias wh=which

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
    # if you are root, this will not be that useful
    psaj | grep -v "$me" 
}
alias psasyswcl="psasys | wc -l"
function psaf { 
    psa | grep -i $1 
}
function define {
    wn "$1" -over
}
alias wno=define
function ff {
    find . -name "$1" -print
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
alias .b=". ~/.bashrc"
alias listen="netstat -a | grep LISTEN"
alias wcl="wc -l"

alias omg="echo wtf"
alias source=.
alias .b=". ~/.profile"

###################
# Global Settings #
###################

# set my editor to be correct
# (the -nw tells it not to open up a new window)
myeditor="emacs"
export EDITOR="$myeditor"
export VISUAL="$myeditor"
export FSEDIT="$myeditor"

# These functions are not supported under ksh!!
#bind "\ep"=history-search-backward
#bind "\en"=history-search-forward

# Setting the default prompt
#export PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "
export PS1="\t \w \$ "


