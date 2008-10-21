# .bashrc

#. ~/doc/dhd/hbase/.sh_ansi_color

uname=`uname`
host=`hostname`
me=`whoami`
menum=`id -u`

## Set the path
#   - not workable if the directory has spaces
#   - put commands that should come before system commands in {,~}/opt/alternatives/
h="${HOME}"
PATH=
d=
d="${d} /usr/local/texlive/2008/bin/universal-darwin"
d="${d} /usr/local/ActivePerl-5.10/bin /usr/local/ActivePerl-5.10/sbin"
d="${d} $h/opt/alternatives /opt/alternatives $h/opt/bin $h/opt/sbin"
d="${d} $h/doc/dhd/opt/bin $h/doc/dhd/os/$uname/bin"
d="${d} /sw/bin /sw/sbin /opt/local/bin /opt/local/sbin"
d="${d} /usr/pkg/bin /usr/pkg/sbin"
d="${d} /usr/nekoware/bin /usr/nekoware/sbin /usr/freeware/bin"
d="${d} /opt/csw/bin /opt/csw/sbin /opt/csw/flex/bin /opt/csw/flex/sbin /opt/csw/gcc4/bin"
d="${d} /opt/csw/gcc4/sbin /opt/SUNWspro/bin /opt/SUNWspro/sbin"
d="${d} /opt/gcc.3.3/bin/i586-pc-interix3 /usr/local/MSVisualStudio/bin"
d="${d} /opt/gcc.3.3/bin /opt/ast/bin"
d="${d} /usr/contrib/bin /usr/contrib/win32/bin /usr/examples/admin"
d="${d} /mingw/bin /c/WINDOWS /c/WINDOWS/system32/Wbem /c/WINDOWS/system32 /c/opt/bin"
d="${d} /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin"
d="${d} /usr/games /usr/games/bin /usr/X11R6/bin /usr/X11R6/sbin /usr/bin/X11"

for p in ${d}; do
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

if   [ $( which gls ) ]; then
    cmd_ls=gls
    ls_args="${ls_args} --color"
elif [ $( which colorls ) ]; then 
    cmd_ls=colorls
    ls_args="${ls_args} -G"
fi
if [ $( which gsed ) ]; then cmd_sed=gsed; fi
if [ $( which gdu  ) ]; then cmd_du=gdu;   fi

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
elif [ -d /dev/fs ]; then # SFU/SUA
    export winc="/dev/fs/C"
    export SVN_SSH="/usr/pkg/bin/ssh"
    test -f /usr/examples/win32/aliases.sh && /usr/examples/win32/aliases.sh
    export windows=1
elif [ $uname = "Darwin" ]; then # Mac OS X
    test -r /sw/bin/init.sh && source /sw/bin/init.sh  # fink
elif [ $uname = "SunOS"  ]; then # Solaris
    export CC="/opt/csw/gcc4/bin/gcc"
    psargs="-ef"
elif [ $uname = "OpenBSD" ]; then # obsd
    export PKG_PATH=ftp://ftp.openbsd.org/pub/OpenBSD/4.3/packages/sparc64/
    psargs_user="j"
elif [ $uname = "Linux"   ]; then
    ls_args="${ls_args} --color" # assume GNU ls 
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
alias listen='netstat -a | grep LISTEN'
alias wcl="wc -l"

alias omg="echo wtf"
alias source=.
alias .b=". ~/.profile"

###################
# Other Functions #
###################
# LaTeX stuff:
function blx { #buildlatex
    latex "$1".tex
    dvipdf "$1".dvi "$1".pdf
    open "$1".pdf
}


###################
# Global Settings #
###################

# set my editor to be correct
# (the -nw tells it not to open up a new window)
myeditor="emacs"
export EDITOR="$myeditor"
export VISUAL="$myeditor"
export FSEDIT="$myeditor"

# last character of prompt
if [ $menum = 0 ]; then lcop='#'
else                    lcop='>'
fi

# Setting the default prompt
# Make sure that the PS1 value is surrounded by \[ and \] - otherwise you will get retarded line wrapping problems. mmmmkay?
#export PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "
#export PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "
# COLORS      bold,green           bold,blue         unbold,white
#           \[\033[01;32m\]     \[\033[01;34m\]      \[\033[00m\]
#      PS1="               \u@\h                \w \$             "
export PS1="\[\033[01;37m\]\t \033[01;34m\]\h\[\033[01;37m\]:\[\033[00;32m\]\w \033[01;34m\]$lcop \[\033[00m\]\]"
#                          \t              \h               :               \w              \$
# COLORS:    bold,white         normal,green      bold,blue       normal,white 
#export PS1="$ansi_bold $ansi_fg_white hello $ansi_fg_green sonny $ansi_fg_white $ansi_norm $ "
#export PS1="\t \w \$ "


