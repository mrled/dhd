#!/bin/bash <---- THIS MAKES EMACS DTRT. 
# .bashrc

export UNAME=`uname`
export HOST=`hostname`


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

for e in $( \ 
    ~/opt/alternatives /opt/alternatives \
    ~/opt/bin ~/opt/sbin \
    ~/doc/remote/dhd/hbase/local/bin ~/doc/remote/dhd/os/$uname/bin \
    \
    /sw/bin /sw/sbin /opt/local/bin /opt/local/sbin \
    /usr/pkg/bin /usr/pkg/sbin \
    /usr/nekoware/bin /usr/nekoware/sbin /usr/freeware/bin \
    /opt/csw/bin /opt/csw/sbin /opt/csw/flex/bin /opt/csw/flex/sbin /opt/csw/gcc4/bin /opt/csw/gcc4/sbin \
    /opt/SUNWspro/bin /opt/SUNWspro/sbin \
    \
    /opt/gcc.3.3/bin/i586-pc-interix3 /usr/local/MSVisualStudio/bin \
    /opt/gcc.3.3/bin /opt/ast/bin \
    /usr/contrib/bin /usr/contrib/win32/bin /usr/examples/admin \
    /mingw/bin /c/WINDOWS /c/WINDOWS/system32/Wbem /c/WINDOWS/system32 /c/opt/bin \
    \
    /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin \
    /usr/games /usr/games/bin /usr/X11R6/bin /usr/X11R6/sbin  /usr/bin/X11 \
    )
do 
    test -d "$entry" && export P="$P:$entry"
done

umask 077 #stop reading my files!!
export CVS_RSH="ssh"


#     # ls... the search for GNU
#     set ls = "/bin/ls"
#     if ( -X gls ) set ls = "gls"
#     # GNU sed
#     if ( -X gsed ) alias sed "gsed"
#     # GNU du for the -h option :):):)
#     if ( -X gdu ) alias du gdu

#     ######################

#     if [ test -d /cygdrive ] then    # Cygwin
#         # it inherits the Windows path, so if your Windows path has this set, 
#         #  then it will be wrong:
#         export SVN_SSH="/usr/bin/ssh"
#         # lists.gnu.org/archive/html/help-emacs-windows/2002-10/msg00109.html:
#         export CYGWIN="binmode ntsec stty"	# I don't know what does what here
#         set winc = "/cygdrive/c"
#     elif [ test -d /dev/fs ] then # SFU/SUA
#         set winc = "/dev/fs/C"
#         export SVN_SSH="/usr/pkg/bin/ssh"
#         test -f /usr/examples/win32/aliases.sh && /usr/examples/win32/aliases.sh
#         # the following assume updated rxvt and xterm, as well as /opt/alternatives
#         # in addition, they assume that your Windows fixed font is profont (??)
#         if ( -X rxvt  ) alias rxvt  "rxvt  -bg black -fg white -fn fixed"
#         if ( -X xterm ) alias xterm "xterm -bg black -fg white -fn fixed -sl 10000"
#     else if ( $uname == Darwin ) then # Mac OS X
#         set surf = 'open -f "$argv" /Apps/Net\ -\ Web/Camino.app'
#         test -r /sw/bin/init.csh && source /sw/bin/init.csh  # fink
#     else if ( $uname == SunOS ) then  # Solaris
#         setenv CC "/opt/csw/gcc4/bin/gcc"
#         alias psa ps -ef
#         alias psaf 'ps -ef|grep -i \!*'
#     endif

#     if ( $?winc ) then # we are on Windows, either cygwin or SFU or something
#         # psh. I do all this and then realize that I should just use 
#         # `runwin32 explorer` instead. 
#         set windir =    "$winc/WINDOWS"
#         set wins32 =    "$winc/WINDOWS/system32"
#         set progfiles = "$winc/Program\ Files"
#         set surf =      "$progfiles/Mozilla\ Firefox/firefox.exe"
#         alias explorer  "$windir/explorer.exe" # you *must* use \ paths and quote *everything*
#         alias explore    explorer
#         alias expl       explorer
#         alias hh        "$windir/hh.exe"
#         alias regedit   "$windir/regedit.exe"
#         alias ifconfig  "$wins32/ipconfig.exe"
#         alias mstsc     "$wins32/mstsc.exe"
#         alias fsutil    "$wins32/fsutil.exe"
#         alias net       "$wins32/net.exe"
#         alias netsh     "$wins32/netsh.exe"
#     endif

#     if ( $?surf ) then # we have a web browser!
#         alias surf $surf
#         alias firefox $surf
#         alias stfu $surf
#     endif


##################
# Global Aliases #
##################

alias ppath='echo $PATH | sed "s/:/\n/g"'
alias pupath='echo $PATH | sed "s/:/\n/g" | sort | uniq'
alias logrec='lsl /var/log | grep -v \\.bz2 | grep -v \\.0 | grep "`date +%b\ %d\ %k`"'

alias psa="ps ax"
alias psaf='ps ax|grep -i \!*'

alias ..='cd ..'
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias wh=which

alias ls='ls -hF --color'
alias lsa='ls -ahF --color'
alias lsl='ls -alhF --color'
alias lsli='ls -alhFi --color' # lsl+inodes
alias l1='ls -1hF --color'
alias llm='ls -lahrtF --color' # lists by last mod time

alias pu="pushd"
alias po="popd"
alias ff="find . -name '\!*' -print"
alias trash='mv $argv ~/.Trash/'
alias tailmes='tail -f /var/log/messages'
alias mess="less /var/log/messages"
alias dmesg='dmesg|less'
alias define='wn \!* -over'
alias .b=". ~/.bashrc"
alias listen='netstat -a | grep LISTEN'

# under OS X, `posd` tells you where the topmost Finder window is. (in fink)
# `fdc` changes the directory of the shell to match that of the
# topmost Finder window. This alias is the reverse behavior. 
alias cdf='cd "`posd`"' # requires posd (in fink). 

alias arin='whois -h whois.arin.net'
alias ripe='whois -h whois.ripe.net'
alias apnic='whois -h whois.apnic.net'

alias co_homebase="mkdir -p ~/doc/remote/dhd && svn co http://mrled.org/svn/dhd doc/remote/"
alias omg='echo wtf'

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


# Setting the default prompt
test -e ~/doc/remote/dhd/hbase/.ansi-colors.sh && . ~/doc/remote/dhd/hbase/.ansi-colors.sh
export PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "


