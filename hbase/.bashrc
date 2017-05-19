#!/bin/bash
# .bashrc should contain ONLY statements that apply to interactive shells

export MRL_BASHRC_GUARD=1
if test -z "$MRL_PROFILE_GUARD"; then
    . $HOME/.profile
fi

alias MRLLSCMD="$(command -v ls)"
ls_args="-hF --color"

unames=$(uname -s)
if strcontains "$unames" Cygwin; then
    export CYGWIN="binmode ntsec stty"
elif strcontains "$unames" MSYS; then
    ls_args="${ls_args} --color"
elif strcontains "$unames" FreeBSD; then
    ls_args="-hFG"
elif test "$unames" = Darwin; then
    ls_args="-hFG"
    # Note that this works on X11 even when keyboard shortcuts are disabled in preferences :)
    alias switchx="osascript ~/.dhd/opt/ascript/x11-cmd-tab.ascript"
    # Launch QuickLook from the command line (^c will kill it and return to prompt)
    alias ql='qlmanage -p 2>/dev/null'
    alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
    pman() {
        man -t "$@" | ps2pdf - - | open -g -f -a /Applications/Preview.app
    }
    # fucking quarantine thing
    # could also turn it off completely:
    # defaults write com.apple.LaunchServices LSQuarantine -bool false
    alias unquarantine="xattr -r -d com.apple.quarantine"
fi
unset unames

cmdavail 'ack-grep' && alias ack='ack-grep'
alias ..="cd .."
alias c=clear
alias df="df -h"
alias h=history
alias m=more
alias l=less
alias zl=zless

export LESS="-icdM"

alias wh="type -a"
alias dush='du -sh'

alias psa="ps ax"
alias psaj="ps axj"
psaf() {
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i "$1" | grep -v "grep -i $1"
}

alias ls='MRLLSCMD $ls_args'
alias lsa='MRLLSCMD $ls_args -a'
alias lsl='MRLLSCMD $ls_args -al'
alias lsli='MRLLSCMD $ls_args -ali' # lsl+inodes
alias l1='MRLLSCMD $ls_args -1'
alias llm='MRLLSCMD $ls_args -lart' # lsl+ sort by modified time (lastest at bottom)

alias dmesg="dmesg|less"
alias wcl="wc -l"

alias omg="echo wtf"

alias .b='. ~/.bashrc'

cmdavail "xscreensaver-command" && alias xslock="xscreensaver-command -lock"

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

alias scrl="screen -list"
alias scrw="screen -wipe"

##
## Remote Commands
##

# this way it won't save ssh host keys to ~/.ssh/known_hosts
alias sshtel="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scptel="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

uploadid() {
    ssh "$@" 'mkdir -p ~/.ssh && cat - >> ~/.ssh/authorized_keys' < "$HOME/.ssh/id_rsa.pub"
}

alias canhazip='curl icanhazip.com'

# gets public ip via dns. can help for when behind pay hotspots.
# via https://twitter.com/climagic/status/220977468360765442 / @brimston3
# some hotspots fuck with udp/53; you might try dig +tcp if that happens
alias dnsip='dig myip.opendns.com  @resolver1.opendns.com +short'

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

# Mac metadata files: .DS_Store and ._Doomsday.mkv for example
mmf() {
    case $1 in
        list)
            find . -type f -name '._*'
            find . -type f -name '.DS_Store';;
        rm)
            find . -type f -name '._*' -exec rm {} \;
            find . -type f -name '.DS_Store' -exec rm {} \;
            ;;
    esac
}

bash_listens() {
    netstat -an | grep -E '((tcp)|(udp)).*LISTEN' | awk '{ print $1, "\t", $4 }' | sort
}
lsof_listens() {
    lsof +M -iTCP -sTCP:LISTEN
    lsof +M -iUDP
}

strip_comments() {
    for file in "$@"; do
        grep -v '^[ | ]*#' "$file" | grep -v '^[    | ]*$'
    done
}

if cmdavail diceware; then
    alias passphrase="diceware --no-caps -d ' ' -n 6"
fi

# Note that while these are commonplace, they are *not* POSIX
export HISTSIZE="INFINITE"
export HISTFILESIZE="INFINITE"

# glob filenames in a case-insensitive manner
# NOT the same as tab-complete case insensitively - you must add
#   set completion-ignore-case on
# in .inputrc for that.
shopt -s nocaseglob
# Fix some problems where lines wrap incorrectly
shopt -s checkwinsize

#### The prompt

# lcop = last character of prompt
if test "$(id -u)" = 0; then lcop='#'; else lcop='>'; fi

# Make sure that all of the non-printing characters in $PS1 are surrounded by
# \[ and \] - otherwise you will get line wrapping problems (even if the
# checkwinsize option is enabled as above)
boldwhite="\[\e[01;37m\]"
boldblue="\[\e[01;34m\]"
normalgreen="\[\e[00;32m\]"
clearrformat="\[\e[00m\]"
export PS1="${boldwhite}\t ${boldblue}\h${boldwhite}:${normalgreen}\W ${boldblue}${lcop} ${clearrformat}"

unset lcop

if test -d "$HOME/.bashrc.d"; then
    for script in $(find "$HOME/.bashrc.d" -type f); do
        . "$script"
    done
fi
