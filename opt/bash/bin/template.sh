#!/bin/bash

set -u  # Exit immediately if a command files
set -e  # Treat unset variables as errors

usage() {
    cat << USAGE
template() [-h|--help]
A template for new Bash scripts
    -h: Print help anbd exit

**IN GENERAL, PLEASE DO NOT WRITE NEW BASH SCRIPTS**
Bash scripts are only appropriate if:
- The script will only be run in a Bash environment (typically Unix, although possibly mingw)
- The script is less than one single solitary PgDn
- The script deals heavily with shell issues
- The script does not rely on third party external tools
USAGE
}

# First get options - that is, arguments preceded with a dash like "-h" or "--help"
# Then get positional arguments - that is, anything not preceded with a dash
i=0; pctr=0; argcount=$#; declare -a posargs; remote=false

while [ $i -lt $argcount ]; do
    case "$1" in
        -h | --help )
            usage
            return
            ;;
        -d | --debug )
            # Show each line before executing
            set -x
            # Here we have passed a bare option, so we consume only one argument from $argv
           ((i++))
            shift
            ;;
        -1 | --one-argument )
            # Here we have passed an option that takes a parameter, so we consume 2 arguments from $argv
            # (This can of course be expanded to take N parameters, and consume N+1 parameters from $argv)
            one_argument_value=$2
            ((i+=2))
            shift 2
            ;;
        --)
            # Here we gather all remaining arguments into an array
            declare -a sa
            sctr=0
            shift
            while [ $i -lt $argcount ]; do
                sa[$sctr]=$1
                ((sctr++))
                ((i++))
                shift
            done
            ;;
        *)
            # For any other cases, check to see if the first character is a dash
            # If so, give an error; if not, collect them into an array of positional arguments
            [ $1 ] && if [ ${1:0:1} == "-" ]; then 
                # for the syntax see e.g.:
                # http://www.softpanorama.org/Scripting/Shellorama/Reference/string_operations_in_shell.shtml
                echo "Error: you supplied option '$1', but there is no such option"
                scr_help
                return
            fi
            posargs[pctr]=$1; ((pctr++)); ((i++)); 
            shift;;
    esac
done
