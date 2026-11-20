#!/bin/sh

# Profile guidelines
# 1. When something should only affect an interactive shell,
#    use the interactive shell's RC file - for Bash, this is .bashrc
# 2. When something should affect noninteractive shells, use this file
#    (.profile)
# 3. The .profile script should be compatible with any POSIX shell -
#    do not use bashisms
# 4. Where possible, place functions in separate files in a directory
#    found in the $PATH rather than defining them in a shell startup script

MRL_PROFILE_GUARD=1

umask 077

# We use this to find paths inside dhd
export DHD=${DHD:-"$HOME/.dhd"}

# Locale settings
# Exactly what all of these do is system-specific.
#
# LC_ALL will *override* the other LC_* values.
# Systems do (should) not set it by default, and you shouldn't set it either.
# export LC_ALL=en_US.UTF-8
#
# LANG is the default locale for all categories
export LANG=en_US.UTF-8
#
# LC_COLLATE controls how information is presented/sorted/etc.
# Affects ls, sort, shell glob expansion order, and other programs using locale-aware string comparisons.
# Values:
#   en_US.UTF-8:
#     On macOS, sort all punnctuation first, then case-insensitive letters.
#     On GNU, ignore punctuation for sorting purposes;
#     I hate this because it means that dotfiles don't come first in directory listings.
#   C.UTF-8:
#     Sorts strings according to bytewise order,
#     including capitals before lower case.
export LC_COLLATE="C.UTF-8"
#
# LC_TIME controls date/time presentation.
# Does NOT affect ls. Does affect date.
#   Show values:
#     for lctime in en_US.UTF-8 en_GB.UTF-8 en_DK.UTF-8; do
#       echo "$lctime: $(LC_TIME=$lctime date --date=@2147483647)"
#     done
#   On GNU:
#     en_US.UTF-8: Mon Jan 18 09:14:07 PM CST 2038
#     en_GB.UTF-8: Mon 18 Jan 21:14:07 CST 2038
#     en_DK.UTF-8: 2038-01-18T21:14:07 CST
#   macOS:
#     en_US.UTF-8: Mon Jan 18 21:14:07 CST 2038
#     en_GB.UTF-8: Mon 18 Jan 2038 21:14:07 CST
# en_DK.UTF-8 is a special case in glibc and doesn't exist on macOS.
export LC_TIME="en_GB.UTF-8"

export SHDETECT_RESULTS="$HOME/.shdetect_dhd.sh"
if test -e "$SHDETECT_RESULTS"; then
    . "$SHDETECT_RESULTS"
else
    echo "WARNING: Missing shdetect results at $SHDETECT_RESULTS" >&2
    echo "WARNING: run $DHD/opt/bin/dhd-shdetect" >&2
fi


export PYTHONSTARTUP=$DHD/hbase/python.profile

# Disable Apple's zsh session restore gunk
export SHELL_SESSIONS_DISABLE=1

# rustup configures this
test -e "$HOME/.cargo/env" && . "$HOME/.cargo/env"

# LESSOPEN is not always set with security in mind, especially on desktop distros
# See also: https://marc.info/?l=full-disclosure&m=141678420425808&w=2
export LESSOPEN=

# Support a shared SSH agent socket.
# This is useful for mosh connections.
# If ~/.ssh/sharedagent.sock exists and points to a valid socket, use it
if test -S "$HOME/.ssh/sharedagent.sock"; then
    export SSH_AUTH_SOCK="$HOME/.ssh/sharedagent.sock"
fi

# fucking Perl/CPAN
export PERL_MM_USE_DEFAULT=1

if test -e "$DHD/hosts/$DHD_HOSTNAME/profile.sh"; then
    . "$DHD/hosts/$DHD_HOSTNAME/profile.sh"
fi

# Source shell-specific init files, but only if we have run shdetect.
# Shell-specific init files like bashrc assume functioning dhd, because they are stored in dhd.
if test "$DHD_SHDETECT_INCLUDED"; then
    if test -z "$MRL_BASHRC_GUARD" && test "$BASH_VERSION"; then
        . "$DHD/hbase/.bashrc"
    elif test -z "$MRL_ZSHRC_GUARD" && test "$ZSH_NAME"; then
        . "$DHD/hbase/.zshrc"
    fi
fi

export DOCKER_CLI_HINTS=false

export HOMEBREW_NO_INSTALL_CLEANUP=1
export HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1

if test -d "$HOME/.profile.d"; then
    for script in "$HOME/.profile.d"/*; do
        test -f "$script" && . "$script"
    done
fi
