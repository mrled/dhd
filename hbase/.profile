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

# A list of all the paths which MIGHT exist and contain binaries we
# want in our $PATH
# Order is important in $PATH; earlier entries are searched first
export POSSIBLE_PATHS="
${HOME}/opt/lib/miniconda3/bin
${HOME}/opt/bin
${HOME}/opt/sbin
${DHD}/opt/bin
${DHD}/opt/bash/bin
${HOME}/opt/homebrew/bin
${HOME}/opt/homebrew/sbin
${HOME}/test spaces in path
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

if cmdavail brew; then
    # This is NOT a standard environment variable, for some reason
    export HOMEBREWDIR=$(dirname "$(dirname "$(command -v brew)")")

    if test -d "$HOMEBREWDIR/opt/go" && test -z "$GOROOT"; then
        export GOROOT="$HOMEBREWDIR/opt/go/libexec"
    fi
fi

export PATH="$(pathsetup "${PATH}:${GOROOT}/bin:${GOPATH}/bin")"

if cmdavail ruby; then
    export PATH="$(pathsetup "${PATH}:$(ruby -rubygems -e 'puts "%s/bin:%s/bin" % [Gem.dir, Gem.user_dir]')")"
fi

export PYTHONSTARTUP=$DHD/hbase/python.profile

export EDITOR=emacs
export VISUAL=emacs
export FSEDIT=emacs

# fucking Perl/CPAN
export PERL_MM_USE_DEFAULT=1

if test -z "$MRL_BASHRC_GUARD" && test "$BASH_VERSION"; then
    . "$DHD/hbase
    /.bashrc"
fi
