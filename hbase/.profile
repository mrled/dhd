#!/bin/sh

# Profile guidelines
# 1. When something should only affect an interactive shell, use the interactive shell's RC file - for Bash, this is .bashrc
# 2. When something should affect noninteractive shells, use this file (.profile)
# 3. The .profile script should be compatible with any POSIX shell - do not use bashisms
# 4. Where possible, place functions in separate files in a directory found in the $PATH rather than defining them in a shell startup script

export MRL_PROFILE_GUARD=1

umask 077

# If set to a filename, log all debugging messages to that file
# If unset, do nothing
# I considered logging to STDERR in color, but that doesn't work in subshells
# Make sure this is set to an absolute path
export PROFDBGLOG=
# export PROFDBGLOG=$HOME/profiledbg.log
profdbglog() {
    if test $PROFDBGLOG; then
        echo "$@" >> $PROFDBGLOG
    fi
}
profdbglog "Starting shell at $(date)"

# Must be in .profile - used to populate the $PATH
pathsetup() {
    if test "$1" = "-h"; then
        cat <<ENDUSAGE
pathsetup(): Set up a PATH-like variable
Given a PATH-like variable, return a PATH-like variable that contains only
extant directories
Usage: pathsetup PATHVAR
    PATHVAR: A PATH-like variable - that is, a variable containing paths
             separated by colons
EXAMPLES
> pathsetup "/bin:/nonexistent:/sbin"
/bin:/sbin
ENDUSAGE
        return
    fi
    pathlike=$1
    outpath=
    initialifs=$IFS
    IFS=:
    for path in $pathlike; do
        if test -d "$path"; then
            profdbglog "Found path: $path"
            foundexisting=
            for existingpath in $outpath; do
                if test "$path" = "$existingpath"; then
                    profdbglog "... however, '$path' was already in output, skipping..."
                    foundexisting=1
                    break
                else
                    profdbglog "... and it did not match with existing path '$existingpath'"
                fi
            done
            test $foundexisting || outpath="${outpath}${path}:"
        else
            profdbglog "Nonexistent path: $path"
        fi
    done
    IFS=$initialifs
    unset pathlike initialifs
    echo "$outpath"
}

d=$PATH
d="${d}:$HOME/opt/bin:$HOME/opt/sbin:$HOME/.dhd/opt/bin:$HOME/.dhd/opt/bash/bin"
d="${d}:$HOME/opt/homebrew/bin:$HOME/opt/homebrew/sbin"
d="${d}:/opt/homebrew/bin:/opt/homebrew/sbin"
d="${d}:/usr/pkg/bin:/usr/pkg/sbin"
d="${d}:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
d="${d}:/c/WINDOWS:/c/WINDOWS/system32/Wbem:/c/WINDOWS/system32"
d="${d}:/c/MinGW/bin:/c/MinGW/sbin:/c/MinGW/msys/1.0/bin:/c/MinGW/msys/1.0/sbin"
d="${d}:$HOME/test path"

PATH=$(pathsetup "$d")
export PATH
unset d

if cmdavail brew; then
    # This is NOT a standard environment variable, for some reason
    HOMEBREWDIR=$(dirname "$(dirname "$(command -v brew)")")
    export HOMEBREWDIR
fi

if cmdavail ruby; then
    usrdir=$(ruby -rubygems -e 'puts Gem.user_dir')
    sysdir=$(ruby -rubygems -e 'puts Gem.dir')
    if test -d "$usrdir"; then PATH="${PATH}:$usrdir/bin"; fi
    if test -d "$sysdir"; then PATH="${PATH}:$sysdir/bin"; fi
    unset usrdir sysdir
fi

if test -d "$HOMEBREWDIR/opt/go"; then
    export GOROOT="$HOMEBREWDIR/opt/go/libexec"
    export GOPATH="$HOME/Documents/Go"
    export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"
fi

export PYTHONSTARTUP=$HOME/.dhd/hbase/python.profile

pp="${PYTHONPATH}:${HOME}/.dhd/opt/python:${HOME}/opt/lib/python3.4/site-packages/"
export PYTHONPATH=pathsetup pp
unset pp

export EDITOR=emacs
export VISUAL=emacs
export FSEDIT=emacs

# fucking Perl/CPAN
export PERL_MM_USE_DEFAULT=1

if test -z "$MRL_BASHRC_GUARD" && test "$BASH_VERSION"; then
    . "$HOME/.bashrc"
fi
