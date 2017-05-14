#!/bin/sh

set -e  # Exit immediately if a command files
set -u  # Treat unset variables as errors

usage() {
    cat << ENDUSAGE
Usage: $0 [-h] [-d] [-1 arg1] [-2 arg1 arg2] [-e] [POSITIONALARGS...]
A template for new POSIX shell scripts
    -h | --help: Print help and exit
    -d | --debug: show debug messages
    -1 | --one-argument: consume the next input argument
    -2 | --two-arguments: consume the next two input arguments
    -e | --exit-with-error: exit with an error
    POSITIONALARGS: printed at the end

**IN GENERAL, PLEASE DO NOT WRITE NEW SHELL SCRIPTS**
Shell scripts are only appropriate if:
- The script will only be run in a POSIX sh environment (Unix, MinGW, etc)
- The script can be written with no bashisms or other non-POSIX extensions
- The script is less than one single solitary PgDn
- The script deals heavily with shell issues
- The script does not rely on non-POSIX third party commands

Some notes
- Why no bashisms? Portability
  Under many circumstances, bash is not /bin/sh, and may not even be installed
  For example: Alpine Linux and Ubuntu ship the Almquist shell (ash) as /bin/sh
  A script that requires bashisms is a script that should be rewritten in a
  more powerful and more readable programming language
- 'set -o pipefile' is a bashism, and not a stanard POSIX option
  In bash, that will fail the whole pipeline if a piped command exits with a
  nonzero return value
  However, that is not ideal because a command may exit with a nonzero return
  value intentionally based on the command it is piped to
  For instance, the 'cat' command in 'cat /etc/profile | head 1' exits with a
  nonzero return value because 'head' sends SIGPIPE to 'cat'
  See also http://cfajohnson.com/shell/cus-faq-2.html#Q11
- Variable capitalization:
  Exported variables should be in all caps ('export VAR=value')
  Script local variables should be in lower case ('var=value')
  See also http://stackoverflow.com/questions/673055/
- Rely on the PATH variable
  Do not assume that binaries are necessarily inside /bin or /usr/bin
  For instance, Alpine Linux has /bin/true and /bin/false, but macOS has
  /usr/bin/true and /usr/bin/false
- Refer to the POSIX documentation when writing scripts
  Official shell documentation can be found here:
  http://pubs.opengroup.org/onlinepubs/9699919799/
  A more easily navigable set of links to this documentation can be found here:
  http://shellhaters.org
  That site also contains an embedded YouTube video of an excellent talk by
  Ryan Tomayko:
  https://www.youtube.com/watch?v=olH-9b3VJfs
- Use ShellCheck to check for POSIX compitibility
  https://www.shellcheck.net
  https://github.com/koalaman/shellcheck
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
debug=false

if test $# = 0; then
    # If we do not provide an argument, show help and exit with error
    usage
    exit 1
fi

while test $ctr -lt $#; do
    case "$1" in
        -h | --help )
            usage
            # If we pass the help argument intentionally, exit without error
            exit 0
            ;;
        -d | --debug )
            # A bare option: consume only 1 argument from $@
            set -x # Show each line before executing
            ctr=$((ctr+1))
            shift
            debug=true
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
        -e | --exit-with-error )
            # When exiting for an error, use a value greater than zero
            exit 1
            ;;
        *)
            ctr=$((ctr+1))
            ;;
    esac
done

if $debug; then echo "Debug mode enabled"; else echo "Debug mode disabled"; fi
echo "Passed $# arguments; processed $ctr arguments"
echo "oneargval = $oneargval, twoarg1 = $twoarg1, twoarg2 = $twoarg2"

# NOTE: unless the $@ array is quoted in the for loop, sh will split quoted
# arguments on spaces. For instance, say the script is called like this:
#   script.sh "one" "two three"
# If $@ is unquoted, the for loop will iterate THREE times, with the $arg
# variable as "one", "two", and "three".
# However, if "$@" is quoted, the loop will iterate TWO times, with the $arg
# variable as "one" and "two three".
echo "All remaining positional arguments:"
for arg in "$@"; do
    echo "- $arg"
done
