# .bashrc

#### Set the path
# 
# Set the path to inclode only directories that exist on this system.
#   - not workable if the directory has spaces (!!)
#   - put commands that should come before system commands in {,~}/opt/alternatives/

h="${HOME}"
PATH=
d=
d="${d} $h/opt/bin $h/opt/sbin $h/.dhd/opt/bin $h/.dhd/opt/bash/bin"
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
unset d h

if type -P ruby >/dev/null; then
    usrdir=$(ruby -rubygems -e 'puts Gem.user_dir')
    sysdir=$(ruby -rubygems -e 'puts Gem.dir')
    if [ -d $usrdir ]; then PATH="${PATH}:$usrdir/bin"; fi
    if [ -d $sysdir ]; then PATH="${PATH}:$sysdir/bin"; fi
    unset usrdir sysdir
fi

# golang
if [ -d "$HOME/opt/homebrew/opt/go" ]; then
    export GOROOT="$HOME/opt/homebrew/opt/go/libexec"
    export GOPATH="$HOME/Documents/Go"
    export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"
fi

umask 077

cmd_ls="ls"
ls_args="-hF --color"

if type -P 'ack-grep' >/dev/null; then alias ack='ack-grep'; fi

if [ -d /cygdrive ]; then    # Cygwin
    export CYGWIN="binmode ntsec stty"
elif [[ $OSTYPE == mingw ]]; then
    ls_args="${ls_args} --color"
elif [[ $OSTYPE == freebsd* ]]; then
    ls_args="-hFG"
elif [[ $OSTYPE == darwin* ]]; then # Mac OS X
    ls_args="-hFG"
    # Note that this works on X11 even when keyboard shortcuts are disabled in preferences :)
    alias switchx="osascript ~/.dhd/opt/ascript/x11-cmd-tab.ascript"
    # Launch QuickLook from the command line (^c will kill it and return to prompt)
    alias ql='qlmanage -p 2>/dev/null'
    alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
    pman() {
        man -t $* | ps2pdf - - | open -g -f -a /Applications/Preview.app 
    }
    # fucking quarantine thing
    # could also turn it off completely: 
    # defaults write com.apple.LaunchServices LSQuarantine -bool false
    alias unquarantine="xattr -r -d com.apple.quarantine"
fi

alias ..="cd .."
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias zl=zless
LESS="-icdM"
alias wh="type -a"
alias dush='du -sh'

# print the path, one item per line
alias ppath='echo $PATH | sed "s/:/\n/g"' 
alias logrec='lsl /var/log | grep -v \\.bz2 | grep -v \\.0 | grep "`date +%b\ %d\ %k`"'

alias psa="ps ax"
alias psaj="ps axj"
psaf() { 
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i $1 | grep -v "grep -i $1"
}

alias .b='. ~/.bashrc'

alias ls="$cmd_ls $ls_args"
alias lsa="$cmd_ls $ls_args -a"
alias lsl="$cmd_ls $ls_args -al"
alias lsli="$cmd_ls $ls_args -ali" # lsl+inodes
alias l1="$cmd_ls $ls_args -1"
alias llm="$cmd_ls $ls_args -lart" # lsl+ sort by modified time (lastest at bottom)

alias dmesg="dmesg|less"
alias wcl="wc -l"

alias omg="echo wtf"

alias grep="grep --color=auto"

if type -P xscreensaver-command >/dev/null; then alias xslock="xscreensaver-command -lock"; fi

xttitle() {
    echo -e "\e]2;""$1""\007"
}
alias xtt=xtermtitle

alias ddate="date +%Y%m%d"

# screen stuff

# Totally arbitrary default session name
# Where I can, such as in "scr", this is pulled from this environment variable
# Note however that in some places, such as .xsession-strumpwm, it's hard-coded to match this value
export default_session_name="camelot"

# Creates an Xterm window title of user@host <screen session name>, but only if running inside screen
if [ $STY ]; then
    # $STY looks like 123123.camelot; just grab the text name and ignore the number:
    session_name=${STY#*.} 
    screen_window_hardstatus="${USER}@${HOSTNAME} <${session_name}>"
    # /bin/echo -ne "\e]2;${screen_window_hardstatus}\007"
fi

alias scrl="screen -list"
alias scrw="screen -wipe"

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

alias canhazip='curl icanhazip.com'

# gets public ip via dns. can help for when behind pay hotspots. 
# via https://twitter.com/climagic/status/220977468360765442 / @brimston3
# some hotspots fuck with udp/53; you might try dig +tcp if that happens
alias dnsip='dig myip.opendns.com  @resolver1.opendns.com +short' 

# this is basically the function that man uses to view its manpages
# if you know the path to a manpage file (like /usr/share/man/man1/ls.1)
# you can view it directly with this function.
alias manualman="groff -Tascii -pet -mandoc -P-c '$1' | less -irs"

# Colorize man pages
# See: http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
man() {
    env \
        LESS_TERMCAP_md=$'\e[1;36m' \
        LESS_TERMCAP_me=$'\e[0m' \
        LESS_TERMCAP_se=$'\e[0m' \
        LESS_TERMCAP_so=$'\e[1;40;92m' \
        LESS_TERMCAP_ue=$'\e[0m' \
        LESS_TERMCAP_us=$'\e[1;32m' \
            man "$@"
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

bash_listens() {
    netstat -an | egrep '((tcp)|(udp)).*LISTEN' | awk '{ print $1, "\t", $4 }' | sort
}
lsof_listens() {
    lsof +M -iTCP -sTCP:LISTEN
    lsof +M -iUDP
}

strip_comments() { 
    for f in $@; do
        grep -v '^[	| ]*#'  $f | grep -v '^[	| ]*$' 
        #grep -v '^[:blank:]*#' $f | grep -v '^[:blank:]*$'
    done
}

if type -p diceware >/dev/null; then
    alias dockerdir="mkdir -v `diceware --no-caps -d - -n 2`"
    alias passphrase="diceware --no-caps -d ' ' -n 6"
fi

export PYTHONSTARTUP=~/.dhd/hbase/python.profile
#export IPYTHONDIR=~/.dhd/hbase/ipython
# fuck python2
if type -P ipython3  >/dev/null; then
    alias ipy=ipython3
    alias ipython=ipython3
fi

pp=
pp="${pp} ${HOME}/.dhd/opt/python"
pp="${pp} ${HOME}/opt/lib/python3.4/site-packages/"
for p in ${pp}; do
    if [ -d ${p} ]; then PYTHONPATH="${PYTHONPATH}${p}:"; fi
done
export PYTHONPATH
unset pp

export HISTSIZE="INFINITE"
export HISTFILESIZE="INFINITE"

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

#### The prompt

# lcop = last character of prompt
if   [ $UID = 0 ]; then lcop='#'; else lcop='>'; fi

# Make sure that all of the non-printing characters in $PS1 are surrounded by
# \[ and \] - otherwise you will get line wrapping problems
# wrapping problems. mmmmkay?
boldwhite="\[\e[01;37m\]"
boldblue="\[\e[01;34m\]"
normalgreen="\[\e[00;32m\]"
clearrformat="\[\e[00m\]"
export PS1="${boldwhite}\t ${boldblue}\h${boldwhite}:${normalgreen}\W ${boldblue}${lcop} ${clearrformat}"

unset lcop
