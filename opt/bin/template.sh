#!/bin/sh

set -e  # Exit immediately if a command files
set -u  # Treat unset variables as errors
set -o pipefail  # If a piped command fails, fail the whole pipeline

usage() {
    cat << ENDUSAGE
$0 [-h|--help]
A template for new shell scripts
    -h: Print help and exit

**IN GENERAL, PLEASE DO NOT WRITE NEW SHELL SCRIPTS**
Shell scripts are only appropriate if:
- The script will only be run in a POSIX sh environment
  (typically Unix, although possibly mingw)
- The script can be written for POSIX sh, with no bash-isms
  (these indicate it should be written in a better, more powerful language)
- The script is less than one single solitary PgDn
- The script deals heavily with shell issues
- The script does not rely on third party external tools
ENDUSAGE
}

# Collect arguments:
# Known flags are collected and acted on first, and any positional arguments
# are kept to the end
#   script.sh -1 onething positional  # Call your script like this
#   script.sh positional -1 onething  # This will NOT work!
# In the first case, 'positional' will be the only positional argument
# In the second case, 'positional', '-1', and 'onething' are all treated as
# positional arguments
ctr=0
oneargval=default
twoarg1=default
twoarg2=default
debug=/bin/false
while [ $ctr -lt $# ]; do
    case "$1" in
        -h | --help )
            usage
            return
            ;;
        -d | --debug )
            # A bare option: consume only 1 argument from $@
            set -x # Show each line before executing
            ctr=$((ctr+1))
            shift
            debug=/bin/true
            ;;
        -1 | --one-argument )
            # An option that takes 1 argument: consume 2 arguments from $@
            oneargval=$2
            ctr=$((ctr+2))
            shift 2
            ;;
        -2 | --two-arguments )
            # An option that takes 2 arguments: consume 3 arguments from $@
            twoarg1=$2
            twoarg2=$3
            ctr=$((ctr+3))
            shift 3
            ;;
        *)
            ctr=$((ctr+1))
            ;;
    esac
done

if $debug; then echo "Debug mode enabled"; else echo "Debug mode disabled"; fi
echo "Processed $ctr arguments"
echo "oneargval = $oneargval, twoarg1 = $twoarg1, twoarg2 = $twoarg2"
echo "All remaining positional arguments: $@"
