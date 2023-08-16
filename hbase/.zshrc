#!/bin/bash

# .zshrc expects that shdetect has already been run.
# .zshrc expects a working ~/.dhd checkout and will dot-source dhd's .profile
# if that hasn't already happened.
# If shdetect isn't working after the .profile dot-source, it will stop execution.
#
# .zshrc should contain ONLY statements that apply to interactive zsh shells
# zshisms (including zsh-supported bashisms) are obviously appropriate here
# Try not to rely on non-POSIX tools (like GNU grep), or that a given optional
# package (like SSH) is installed.
# An exception: when it's more helpful for a command to fail than for it to be
# undefined
# If possible, test for non-POSIX extensions before using in this file
# This file may be sourced multiple times; all settings should be idempotent

# We use this to find paths inside dhd
export DHD=${DHD:-"$HOME/.dhd"}

MRL_ZSHRC_GUARD=1
if test -z "$MRL_PROFILE_GUARD"; then
    . $DHD/hbase/.profile
fi

if test -z "$DHD_SHDETECT_INCLUDED"; then
    return
fi

interactive=
case $- in
    *i*) interactive=1;;
esac

# Ignore the rest of this file if this shell is noninteractive
# We only care about this file in an interactive shell
# ... probably I should reorganize this shit but whatever
if ! test "$interactive"; then
    return
fi

# Use Emacs line editing
bindkey -e

# zsh forward-word does something different than bash and emacs do by default.
# The simplest example of the difference: zsh treats words with hyphens like
# "select-word-style" as a single word, so alt-f jumps from the beginning to the end,
# while bash/emacs treats this as 3 separate words, so alt-f jumps from
# the beginning of 'select' to the dash between 'select' and 'word'.
# Here we make zfs work like bash, since that's where I'm coming from.
autoload -U select-word-style
select-word-style bash

export CDPATH="$HOME:$HOME/Documents:$HOME/Documents/Repositories"

# A pipeline with a failing command at the beginning will set $? to a failure even if later piped commands succeed
set -o pipefail

# Required for unsetting CASE_GLOB
setopt extendedglob

# glob filenames in a case-insensitive manner
# NOT the same as tab-complete case insensitively - you must add
#   set completion-ignore-case on
# in .inputrc for that.
unsetopt CASE_GLOB

# When writing the history file, append this session's commands to the file
# (By default, it will overwrite the history file with commands from this session)
setopt APPEND_HISTORY

case "$DHD_LS_TYPE" in
    gnu) alias ls="ls -FhN --color=always";;
    bsd) alias ls="ls -FhG";;
    *) alias ls="ls -F";;
esac
alias lsa='ls -a'
alias lsl='ls -a -l'
alias lsli='lsl -i' # lsl+inodes
alias l1='ls -1'
alias llm='lsl -r -t' # lsl+ sort by modified time (lastest at bottom)

if test "$DHD_DF_H"; then
    alias df="df -h"
fi

alias ..="cd .."
alias c=clear
alias h=history
alias wcl="wc -l"
alias omg="echo wtf"
alias .z='. $HOME/.profile; . $HOME/.zshrc'
alias xttitle='printf "\e]2;""$@""\007"'
alias ddate="date +%Y%m%d"

alias m=more
alias l=less
export LESS="-icdMR"

alias wh="type -a"  # type -a is a bashism
alias dush='du -sh' # du -h is a POSIX extension

# This is dumb but I can never fucking remember that tcsh's rehash is bash's hash -r
# I haven't used tcsh in over a decade jesus christ
alias rehash='hash -r'

# Test for a grep that supports --color=auto, which includes GNU, FreeBSD, and macOS greps
if test "$DHD_LS_SUPPORTS_COLOR_AUTO"; then
    alias grep="grep --color=auto"
fi

alias psa="ps -A"
psaf() {
    # (the second call to grep prevents this function from being returned as a hit)
    psa | grep -i "$1" | grep -v "grep -i $1"
}

# For my 'scr' command
export SCR_DEFAULT_SESSION="megaframe"

# Aliases to make it easy to connect over ssh or scp WITHOUT CHECKING HOST KEYS
alias sshtel="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scptel="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

alias canhazip='curl icanhazip.com'

# gets public ip via dns. can help for when behind pay hotspots.
# via https://twitter.com/climagic/status/220977468360765442 / @brimston3
# some hotspots fuck with udp/53; you might try dig +tcp if that happens
alias dnsip='dig myip.opendns.com  @resolver1.opendns.com +short'

alias k=kubectl

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

# The number of lines to save to _memory_ in a history list
export HISTSIZE="50000"
# The number of lines to save to _disk_ in a history list
export SAVEHIST="5000"

# Completion settings
# case-insensitive completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
# Work like bash with show-all-if-ambiguous in .inputrc
setopt no_auto_menu
# We have to enable bash-compatible completion for at least aws
autoload bashcompinit && bashcompinit
# I don't understand what these do, lol
autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit
# Command-specific completion
dhd_cmdavail doctl && source <(compdef _doctl doctl)
dhd_cmdavail aws && dhd_cmdavail aws_completer && complete -C aws_completer aws
dhd_cmdavail kubectl && source <(kubectl completion zsh)

# A basic prompt
# If starship is installed, it may override this
if [[ $EUID == 0 ]]; then
    lcop="%B%F{red}☭%f%b"
else
    # lcop="%B%F{magenta}❦%f%b"
    # lcop="%B%F{magenta}❧%f%b"
    lcop="%B%F{magenta}∴%f%b"
fi
PROMPT="%B%F{white}%*%f%b E%? %B%F{blue}%m%f%b %F{green}%1~%f $lcop "

# Enable dbhist
DBHISTORY=true
# Location of dbhist database
DBHISTORYFILE=$HOME/.dbhist
# Dot-source dbhist
# This must happen _after_ bashprompt is set
source $DHD/opt/bash/dbhist.sh


if test -d "$HOME/.zshrc.d"; then
    for script in $(find "$HOME/.zshrc.d" -type f); do
        . "$script"
    done
fi

if test -e "$DHD/hosts/$DHD_HOSTNAME/motd.sh"; then
    . "$DHD/hosts/$DHD_HOSTNAME/motd.sh"
else
    . "$DHD/hosts/default/motd.sh"
fi
if test -e "$DHD/hosts/$DHD_HOSTNAME/zshrc.sh"; then
    . "$DHD/hosts/$DHD_HOSTNAME/zshrc.sh"
fi


# Jesus fucking christ these people are just so fucking tacky
export DOCKER_SCAN_SUGGEST=false
export DOCKER_CLI_HINTS=false

# dhd_cmdavail starship && eval "$(starship init zsh)"

# Indeed setup gunk.
# update-managed-repos adds this line, but it runs a lot of code on every shell launch, which is slow.
# Here, we disable it, but we define an alias to allow importing it if necessary

# Disable it:
if test "$MRL_INDEED_ENV_SETUP_GUNK"; then

echo ".zshrc: enabling Indeed env setup gunk"

# BEGIN env Setup -- Managed by Ansible DO NOT EDIT.

# Setup INDEED_ENV_DIR earlier.
if [ -z "${INDEED_ENV_DIR}" ]; then
    export INDEED_ENV_DIR="/Users/mledbetter/env"
fi

# Single-brace syntax because this is required in bash and sh alike
if [ -e "${INDEED_ENV_DIR}/etc/indeedrc" ]; then
    . "${INDEED_ENV_DIR}/etc/indeedrc"
fi
# END env Setup -- Managed by Ansible DO NOT EDIT.

else
    # Allow re-importing zshrc with the above enabled:
    alias indeedgunk="export MRL_INDEED_ENV_SETUP_GUNK=yes; . ~/.profile; . ~/.zshrc"

    # Add some indeed-specific nice-to-haves without requiring the gunk
    alias cdi="cd ~/indeed"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# First this environment variable is set in /etc/zshrc
# Then we go and reset our path because of dhd-shdetect
# Now I'm unsetting this variable so that this script will run
# TODO: fix this the right way
unset __ETC_PROFILE_NIX_SOURCED
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
