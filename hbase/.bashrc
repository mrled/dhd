#!/bin/bash

# .bashrc expects that shdetect has already been run.
# .bashrc expects a working ~/.dhd checkout and will dot-source dhd's .profile
# if that hasn't already happened.
# If shdetect isn't working after the .profile dot-source, it will stop execution.
#
# .bashrc should contain ONLY statements that apply to interactive bash shells
# Bashisms are obviously appropriate here
# Try not to rely on non-POSIX tools (like GNU grep), or that a given optional
# package (like SSH) is installed.
# An exception: when it's more helpful for a command to fail than for it to be
# undefined
# If possible, test for non-POSIX extensions before using in this file
# This file may be sourced multiple times; all settings should be idempotent

# For fuck's sake, Apple
# export BASH_SILENCE_DEPRECATION_WARNING=1

# We use this to find paths inside dhd
export DHD=${DHD:-"$HOME/.dhd"}

MRL_BASHRC_GUARD=1
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


export CDPATH="$HOME:$HOME/Documents:$HOME/Documents/Repositories"

# A pipeline with a failing command at the beginning will set $? to a failure even if later piped commands succeed
set -o pipefail

# glob filenames in a case-insensitive manner
# NOT the same as tab-complete case insensitively - you must add
#   set completion-ignore-case on
# in .inputrc for that.
shopt -s nocaseglob

# Fix some problems where lines wrap incorrectly
shopt -s checkwinsize

# When writing the history file, append this session's commands to the file
# (By default, it will overwrite the history file with commands from this session)
shopt -s histappend

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
alias .b='. $HOME/.profile; . $HOME/.bashrc'
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

cdd() {
    if test $# -ne 2; then

        cat <<ENDUSAGE
Usage: cdd [-h] <PATTERN> <REPLACEMENT>
Replace a segment of the CWD matched by PATTERN with REPLACEMENT

ARGUMENTS
    -h | --help:    Print help and exit
    PATTERN:        A pattern in the current CWD to replace
    REPLACEMENT:    The replacement for PATTERN

EXAMPLES
    [~/projects/foobar/src/lib/whatever/]$ cdd foobar bazbar
    [~/projects/bazbar/src/lib/whatever/]$

NOTES
    From: https://mobile.twitter.com/frameslip/status/987878911643013120
    This _must_ be a shell function/alias because it uses 'cd'
ENDUSAGE

        return
    fi
    cd "$( pwd | sed -e s/"$1"/"$2"/ )"
}

# The number of lines to save to _memory_ in a history list
export HISTSIZE="INFINITE"
# The number of lines to save to _disk_ in a history list
# Formerly INFINITE, set to finite value to prevent secrets from persisting forever
export HISTFILESIZE="5000"
# If a command starts with a space, do not save it in history
export HISTCONTROL="ignorespace"
# Add a timestamp to history entry
#export HISTTIMEFORMAT="%Y-%m-%d %H:%M:%S "
# Add an epoch-based timestamp to history entry (required for dbhist)
HISTTIMEFORMAT="%s "

# Completion settings
# on macOS, 'brew install bash-completion' is required
test -e $HOMEBREWDIR/etc/bash_completion && source $HOMEBREWDIR/etc/bash_completion
dhd_cmdavail doctl && source <(doctl completion bash)
dhd_cmdavail aws && dhd_cmdavail aws_completer && complete -C aws_completer aws

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
ansi_fg_gray=$(ansi mode=unbold fg=white)
ansi_fg_red=$(ansi fg=red)
ansi_fg_blue=$(ansi fg=blue)
ansi_fg_magenta=$(ansi fg=magenta)
bashprompt() {
    # Gather the exit code first, in case something resets it
    exitcoderaw="$?"

    # Save the shell's history
    history -a

    # Reset any previous settings, in case last output did not
    init="\[${ansi_reset}\]"

    dateraw='\t'
    date="\[${ansi_bold}${ansi_fg_white}\]${dateraw}\[${ansi_reset}\]"

    if test "$exitcoderaw" -eq 0 || test -z "$exitcoderaw"; then
        # Covers a case where no command has been previously executed
        exitcoderaw=0
        exitcodecolor="$ansi_fg_gray"
    else
        exitcodecolor="$ansi_fg_red"
    fi
    exitcode="\[${exitcodecolor}\]E${exitcoderaw}\[${ansi_reset}\]"

    # Allow setting PROMPT_HOSTNAME_OVERRIDE for situations where the hostname
    # is not a useful way to identify the host
    # For instance, the hostname may be automatically generated by Docker
    hostnameraw="${PROMPT_HOSTNAME_OVERRIDE:-"\\h"}"
    hostname="\[${ansi_bold}${ansi_fg_blue}\]$hostnameraw\[$ansi_reset\]"

    jobcountraw=$(jobs | wc -l | sed 's/ *//g')
    if test "$jobcountraw" -gt 0 2> /dev/null; then
        jobcountcolor="$ansi_fg_magenta"
    else
        jobcountcolor="$ansi_fg_gray"
    fi
    jobcount="\[${jobcountcolor}\]J${jobcountraw}\[${ansi_reset}\]"

    workdirraw='\W'
    workdir="\[${ansi_fg_green}\]${workdirraw}\[${ansi_reset}\]"

    # lcop = last character of prompt
    # Use bash's $EUID variable to avoid having to shell out to 'id'
    lcopraw='>'
    if test "$EUID" -eq 0; then
        lcopraw='#'
    fi
    lcop="\[${ansi_bold}${ansi_fg_blue}\]${lcopraw}\[${ansi_reset}\]"

    export PS1="${init}${date} ${exitcode} ${hostname} ${jobcount} ${workdir} ${lcop} "
}

# Currently, Warp can't handle PROMPT_COMMAND
if test "$TERM_PROGRAM" != "WarpTerminal"; then
    export PROMPT_COMMAND=bashprompt
fi

# Enable dbhist
DBHISTORY=true
# Location of dbhist database
DBHISTORYFILE=$HOME/.dbhist
# Dot-source dbhist
# This must happen _after_ bashprompt is set
source $DHD/opt/bash/dbhist.sh


if test -d "$HOME/.bashrc.d"; then
    for script in $(find "$HOME/.bashrc.d" -type f); do
        . "$script"
    done
fi

if test -e "$DHD/hosts/$DHD_HOSTNAME/motd.sh"; then
    . "$DHD/hosts/$DHD_HOSTNAME/motd.sh"
else
    . "$DHD/hosts/default/motd.sh"
fi
if test -e "$DHD/hosts/$DHD_HOSTNAME/bashrc.sh"; then
    . "$DHD/hosts/$DHD_HOSTNAME/bashrc.sh"
fi


PATH="/Users/mrled/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/mrled/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/mrled/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/mrled/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/mrled/perl5"; export PERL_MM_OPT;

# Jesus fucking christ these people are just so fucking tacky
export DOCKER_SCAN_SUGGEST=false


# Indeed setup gunk.
# update-managed-repos adds this line, but it runs a lot of code on every shell launch, which is slow.
# Here, we disable it, but we define an alias to allow importing it if necessary

# Disable it:
if test "$MRL_INDEED_ENV_SETUP_GUNK"; then

echo ".bashrc: enabling Indeed env setup gunk"

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
    # Allow re-importing bashrc with the above enabled:
    alias indeedgunk="export MRL_INDEED_ENV_SETUP_GUNK=yes; . ~/.profile; . ~/.bashrc"

    # Add some indeed-specific nice-to-haves without requiring the gunk
    alias cdi="cd ~/indeed"
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
test -e "$HOME"/.cargo/env && . "$HOME/.cargo/env"

