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

# A list of all the paths which MIGHT exist and contain binaries we
# want in our $PATH
# Order is important in $PATH; earlier entries are searched first
# Some specific notes:
# * npmglobal can be customized: https://docs.npmjs.com/resolving-eacces-permissions-errors-when-installing-packages-globally
export POSSIBLE_PATHS="
${HOME}/opt/lib/miniconda3/bin
${HOME}/opt/bin
${HOME}/opt/sbin
${HOME}/.cabal/bin
${HOME}/.cabal/sbin
${HOME}/.local/bin
${HOME}/.local/sbin
${HOME}/.poetry/bin
${DHD}/opt/bin
${DHD}/opt/bash/bin
${HOME}/opt/homebrew/bin
${HOME}/opt/homebrew/sbin
${HOME}/opt/npmglobal/bin
${HOME}/opt/npmglobal/sbin
${HOME}/test spaces in path
/Library/Frameworks/Python.framework/Versions/3.8/bin
/opt/homebrew/bin
/opt/homebrew/sbin
/usr/pkg/bin
/usr/pkg/sbin
/usr/local/bin
/usr/local/sbin
/usr/bin
/usr/sbin
/bin
/sbin
/c/WINDOWS
/c/WINDOWS/system32/Wbem
/c/WINDOWS/system32
/c/MinGW/bin
/c/MinGW/sbin
/c/MinGW/msys/1.0/bin
/c/MinGW/msys/1.0/sbin
${PATH}
"

export PATH=$("$DHD/opt/bin/pathsetup" "$POSSIBLE_PATHS")

if ! test "$MANPATH"; then
    if cmdavail manpath; then
        export MANPATH=$(manpath)
    else
        export MANPATH="/usr/share/man"
    fi
fi

if cmdavail brew; then
    # This is NOT a standard environment variable, for some reason
    export HOMEBREWDIR=$(dirname "$(dirname "$(command -v brew)")")

    if test -d "$HOMEBREWDIR/opt/go" && test -z "$GOROOT"; then
        export GOROOT="$HOMEBREWDIR/opt/go/libexec"
    fi
fi

if cmdavail npm; then
    export NPM_PACKAGES="${HOME}/.npm-packages"
    npm config set prefix "${HOME}/.npm-packages"
    export PATH="$(pathsetup "${PATH}:${NPM_PACKAGES}/bin")"
    export MANPATH="${MANPATH}:${NPM_PACKAGES}/share/man"
fi

export GOPATH="${HOME}/Documents/Go"

export PATH="$(pathsetup "${PATH}:${GOROOT}/bin:${GOPATH}/bin")"

if cmdavail ruby; then
    export PATH="$(pathsetup "${PATH}:$(ruby -rrubygems -e 'puts "%s/bin:%s/bin" % [Gem.dir, Gem.user_dir]')")"
fi

export PYTHONSTARTUP=$DHD/hbase/python.profile

# LESSOPEN is not always set with security in mind, especially on desktop distros
# See also: https://marc.info/?l=full-disclosure&m=141678420425808&w=2
export LESSOPEN=

if cmdavail vimr; then
    export EDITOR="vimr --wait --cur-env"
elif cmdavail nvim; then
    export EDITOR=nvim
elif cmdavail vim; then
    export EDITOR=vim
else
    export EDITOR=vi
fi
export VISUAL="$EDITOR"
export FSEDIT="$EDITOR"

# fucking Perl/CPAN
export PERL_MM_USE_DEFAULT=1

if test -z "$MRL_BASHRC_GUARD" && test "$BASH_VERSION"; then
    . "$DHD/hbase/.bashrc"
fi

