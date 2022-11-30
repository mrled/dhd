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
    echo "WARNING: run $DHD/opt/bin/dhd-shdetect"
fi

export PYTHONSTARTUP=$DHD/hbase/python.profile

# rustup configures this
test -e "$HOME/.cargo/env" && . "$HOME/.cargo/env"

# LESSOPEN is not always set with security in mind, especially on desktop distros
# See also: https://marc.info/?l=full-disclosure&m=141678420425808&w=2
export LESSOPEN=

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
    fi
fi

export MSDK_HOME=$INDEED_PROJECT_DIR/business-automation/deathstar/server/src/main/webapp/JPMC_MSDK_1.5.0

# Indeed setup gunk.
# update-managed-repos adds this line, but it runs a lot of code on every shell launch, which is slow.
# Here, we disable it, but we define an alias to allow importing it if necessary

# Disable it:
if test "$MRL_INDEED_ENV_SETUP_GUNK"; then

echo ".profile: enabling Indeed env setup gunk"

# BEGIN env Setup -- Managed by Ansible DO NOT EDIT.

# Homebrew uses a /opt/homebrew on Apple Silicon.
eval $(/opt/homebrew/bin/brew shellenv)

# Setup INDEED_ENV_DIR earlier.
if [ -z "${INDEED_ENV_DIR}" ]; then
    export INDEED_ENV_DIR="/Users/mledbetter/env"
fi

# Single-brace syntax because this is required in bash, dash, zsh, etc
if [ -e "${INDEED_ENV_DIR}/etc/indeed_profile" ]; then
    . "${INDEED_ENV_DIR}/etc/indeed_profile"
fi

# Add ~/env/bin to your PATH to use the shared shell scripts from delivery/env
if [ -d "${INDEED_ENV_DIR}/bin" ]; then
    PATH="${INDEED_ENV_DIR}/bin:$PATH"
fi

# On OSX, explicitly source .bashrc so that OS X bash is guaranteed to include all definitions, changing .bashrc to .zshrc for zsh
if [ "Darwin" = "$(uname -s)" ]; then
    [ -s "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi

# END env Setup -- Managed by Ansible DO NOT EDIT.

fi
