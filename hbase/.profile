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

LANG=en_US.UTF-8
LC_ALL="${LANG}"

export GOPATH="${HOME}/Documents/Go"

export SHDETECT_RESULTS="$HOME/.shdetect_dhd.sh"
if test -e "$SHDETECT_RESULTS"; then
    . "$SHDETECT_RESULTS"
else
    echo "WARNING: Missing shdetect results at $SHDETECT_RESULTS"
    echo "WARNING: run $DHD/opt/bin/shdetect"
fi

export PYTHONSTARTUP=$DHD/hbase/python.profile

# LESSOPEN is not always set with security in mind, especially on desktop distros
# See also: https://marc.info/?l=full-disclosure&m=141678420425808&w=2
export LESSOPEN=

# fucking Perl/CPAN
export PERL_MM_USE_DEFAULT=1

# Source shell-specific init files, but only if we have run shdetect.
# Shell-specific init files like bashrc assume functioning dhd, because they are stored in dhd.
if test "$DHD_SHDETECT_INCLUDED"; then
    if test -z "$MRL_BASHRC_GUARD" && test "$BASH_VERSION"; then
        . "$DHD/hbase/.bashrc"
    fi
fi
