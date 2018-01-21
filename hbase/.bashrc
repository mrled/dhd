#!/bin/bash

# .bashrc should contain ONLY statements that apply to interactive shells
# Bashisms are obviously appropriate here
# Try not to rely on non-POSIX tools (like GNU grep), or that a given optional
# package (like SSH) is installed.
# An exception: when it's more helpful for a command to fail than for it to be
# undefined
# If possible, test for non-POSIX extensions before using in this file
# This file may be sourced multiple times; all settings should be idempotent

export MRL_BASHRC_GUARD=1
if test -z "$MRL_PROFILE_GUARD"; then
    . $HOME/.profile
fi

# A pipeline with a failing command at the beginning will set #? to a failure even if later piped commands succeed
set -o pipefail

# glob filenames in a case-insensitive manner
# NOT the same as tab-complete case insensitively - you must add
#   set completion-ignore-case on
# in .inputrc for that.
shopt -s nocaseglob

# Fix some problems where lines wrap incorrectly
shopt -s checkwinsize

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
    alias switchx="osascript $HOME/.dhd/opt/ascript/x11-cmd-tab.ascript"

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
alias .b='. $HOME/.profile; . $HOME/.bashrc'
cmdavail "xscreensaver-command" && alias xslock="xscreensaver-command -lock"
alias xttitle='printf "\e]2;""$@""\007"'
alias ddate="date +%Y%m%d"

alias m=more
alias l=less
cmdavail zless && alias zl=zless
export LESS="-icdMR"

alias wh="type -a"  # type -a is a bashism
alias dush='du -sh' # du -h is a POSIX extension

# This is dumb but I can never fucking remember that tcsh's rehash is bash's hash -r
# I haven't used tcsh in over a decade jesus christ
alias rehash='hash -r'

# Test for a grep that supports --color=auto, which includes GNU, FreeBSD, and macOS greps
grepcmd="grep"
if echo x | grep -q --color=auto x 2>/dev/null; then
    grepcmd="grep --color=auto"
    alias grep=$grepcmd
fi

alias psa="ps -A"
psaf() {
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i "$1" | grep -v "$grepcmd -i $1"
}

# For my 'scr' command
export SCR_DEFAULT_SESSION="megaframe"

# Aliases to make it easy to connect over ssh or scp WITHOUT CHECKING HOST KEYS
alias sshtel="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scptel="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

uploadid() {
    ssh "$@" 'mkdir -p $HOME/.ssh && cat - >> $HOME/.ssh/authorized_keys' < "$HOME/.ssh/id_rsa.pub"
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

# Common, but not POSIX
export HISTSIZE="INFINITE"
export HISTFILESIZE="INFINITE"

# Completion settings
# on macOS, 'brew install bash-completion' is required
test -e $HOMEBREWDIR/etc/bash_completion && source $HOMEBREWDIR/etc/bash_completion
cmdavail doctl && source <(doctl completion bash)
cmdavail aws aws_completer && complete -C aws_completer aws

# Prompt
# NOTE: The bash prompt keeps track of its length, in characters, in order to
# propertly wrap text if the command line being entered gets too long.
# Unfortunately, it does so indiscriminately - even non-printable characters,
# which by definition do not take up space on the line, are counted by
# default.
# Working around this problem requires two components:
# 1. checkwinsize (set above)
# 2. wrapping sequences of non-printable characters in \[ and \]
# My `ansi` script is supposed to handle #2, but unfortunately that isn't
# working the way I expected, so I had to resort to adding the wrapper
# characters myself below.
# Additionally, I set these $ansi_* variables below so that the ansi script is
# only being called once, in order to speed up the prompt.
ansi_reset=$(ansi mode=reset)
ansi_bold=$(ansi mode=bold)
ansi_fg_white=$(ansi fg=white)
ansi_fg_green=$(ansi fg=green)
ansi_fg_red=$(ansi fg=red)
ansi_fg_blue=$(ansi fg=blue)
bashprompt() {
    # Gather the exit code first, in case something resets it
    exitcoderaw="$?"

    # Reset any previous settings, in case last output did not
    init="\[${ansi_reset}\]"

    dateraw='\t'
    date="\[${ansi_bold}${ansi_fg_white}\]${dateraw}\[${ansi_reset}\]"

    if test "$exitcoderaw" -eq 0 || test -z "$exitcoderaw"; then
        # Covers a case where no command has been previously executed
        exitcoderaw=0
        exitcodecolor="$ansi_fg_green"
    else
        exitcodecolor="$ansi_fg_red"
    fi
    exitcode="\[${exitcodecolor}\]${exitcoderaw}\[${ansi_reset}\]"

    # Allow setting PROMPT_HOSTNAME_OVERRIDE for situations where the hostname
    # is not a useful way to identify the host
    # For instance, the hostname may be automatically generated by Docker
    hostnameraw="${PROMPT_HOSTNAME_OVERRIDE:-"\\h"}"
    hostname="\[${ansi_bold}${ansi_fg_blue}\]$hostnameraw\[$ansi_reset\]"

    workdirraw='\W'
    workdir="\[${ansi_fg_green}\]${workdirraw}\[${ansi_reset}\]"

    # lcop = last character of prompt
    # Use bash's $EUID variable to avoid having to shell out to 'id'
    lcopraw='>'
    if test "$EUID" -eq 0; then
        lcopraw='#'
    fi
    lcop="\[${ansi_bold}${ansi_fg_blue}\]${lcopraw}\[${ansi_reset}\]"

    export PS1="${init}${date} ${exitcode} ${hostname}:${workdir} ${lcop} "
}
export PROMPT_COMMAND=bashprompt

if test -d "$HOME/.bashrc.d"; then
    for script in $(find "$HOME/.bashrc.d" -type f); do
        . "$script"
    done
fi
