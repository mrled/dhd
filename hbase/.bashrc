#!/bin/bash

# .bashrc should contain ONLY statements that apply to interactive shells
# Bashisms are obviously appropriate here
# However, do not rely on non-POSIX tools like ls or grep
# An exception: when it's more helpful for an alias to fail than for it to be undefined
# For instance, in the sshtel alias, an error message that 'ssh' is unavailable is more helpful than a message that 'sshtel' is unavailable
# If possible, test for non-POSIX extensions before using in this file
# If that's not possible, mark them in a comment
# Also, this file may be sourced multiple times; all settings should be idempotent

export MRL_BASHRC_GUARD=1
if test -z "$MRL_PROFILE_GUARD"; then
    . $HOME/.profile
fi

# These should be POSIX compatible; extensions are detected later
lscmd=ls
lsargs="-F"
if ls --version 2>/dev/null | grep -q GNU; then
    lsargs="$lsargs -H --color"
elif ls -h -G >/dev/null 2>&1; then
    lsargs="$lsarge -h -G"
fi
alias ls="$lscmd $lsargs"
alias lsa='ls -a'
alias lsl='ls -a -l'
alias lsli='lsl -i' # lsl+inodes
alias l1='ls -1'
alias llm='lsl -r -t' # lsl+ sort by modified time (lastest at bottom)

# System-specific enhancements
# Ideally, don't rely on *too many* of these
unames=$(uname -s)
if strcontains "$unames" Cygwin; then
    export CYGWIN="binmode ntsec stty"
elif test "$unames" = Darwin; then

    alias previewman='man -t "$@" | ps2pdf - - | open -g -f -a /Applications/Preview.app'

    # Note that this works on X11 even when keyboard shortcuts are disabled in preferences :)
    alias switchx="osascript ~/.dhd/opt/ascript/x11-cmd-tab.ascript"

    # Launch QuickLook from the command line (^c will kill it and return to prompt)
    alias ql='qlmanage -p 2>/dev/null'

    # fucking quarantine thing
    # could also turn it off completely:
    # defaults write com.apple.LaunchServices LSQuarantine -bool false
    alias unquarantine="xattr -r -d com.apple.quarantine"
fi
unset unames

cmdavail 'ack-grep' && alias ack='ack-grep'
df -h >/dev/null 2>&1 && alias df="df -h"
alias ..="cd .."
alias c=clear
alias h=history
alias wcl="wc -l"
alias omg="echo wtf"
alias .b='. ~/.profile; . ~/.bashrc'
cmdavail "xscreensaver-command" && alias xslock="xscreensaver-command -lock"
alias xttitle='printf "\e]2;""$@""\007"'
alias ddate="date +%Y%m%d"

alias m=more
alias l=less
cmdavail zless && alias zl=zless
export LESS="-icdMR"

alias wh="type -a"  # type -a is a POSIX extension
alias dush='du -sh' # du -h is a POSIX extension

# Test for GNU grep
if echo x | grep -q --color=auto x 2>/dev/null; then
    grepcmd="grep --color=auto"
    alias grep=$grepcmd
fi

alias psa="ps -A"
psaf() {
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i "$1" | grep -v "$grepcmd $1"
}

# For my 'scr' command
export SCR_DEFAULT_SESSION="camelot"

# Aliases to make it easy to connect over ssh or scp WITHOUT CHECKING HOST KEYS
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

listens() {
    netstat -an | grep -E '((tcp)|(udp)).*LISTEN' | awk '{ print $1, "\t", $4 }' | sort
}

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

# The ANSI escape sequences will work fine in a POSIX shell, in that the shell will pass them directly to the terminal
# Different terminals support different escape sequences, however
# However, bash (and probably any POSIX shell) will detect escape sequences as characters
# This will cause line wrappoing problems when used in the prompt - the shell will think your prompt is longer than it is, meaning that commands will wrap the window early, and editing history can be tricky
# This problem can occur in bash even if the checkwinsize option is enabled (as it is above)
# There is a solution if the shell is based on GNU readline, which bash is: surround escape sequences in \[ \]
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
