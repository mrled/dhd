#!/usr/bin/env python3

import argparse
import logging
import os
import pdb
import subprocess
import sys


scriptdir = os.path.dirname(os.path.realpath(__file__))
logging.basicConfig(level=logging.INFO, format='[%(asctime)s] [%(name)s] [%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)


# Helper functions


strace = pdb.set_trace


def idb_excepthook(type, value, tb):
    """Call an interactive debugger in post-mortem mode

    If you do "sys.excepthook = idb_excepthook", then an interactive debugger
    will be spawned at an unhandled exception
    """
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        # we are in interactive mode or we don't have a tty-like
        # device, so we call the default hook
        sys.__excepthook__(type, value, tb)
    else:
        import traceback
        # we are NOT in interactive mode, print the exception...
        traceback.print_exception(type, value, tb)
        print
        # ...then start the debugger in post-mortem mode.
        pdb.pm()


def pipe(arg_kwarg_list):
    """Construct a shell pipeline

    Invokes the first command in the arglist, retrieves its STDOUT, passes that to the STDIN of the
    next command in the arglist, and so on.

    Logs each command, including its STDIN, STDOUT, and STDERR.

    arg_kwarg_list:     A list of (command, kwargs) tuples
                        command:    A list to pass to subprocess.Popen
                        kwargs:     Any keyword arguments to subprocess.Popen
    result:             The STDOUT of the final command

    Example:

        # Call:
        pipe([
            (['ls', '-1'], {'cwd': '/'}),
            (['head', '-n', '2'], {}),      # Can pass an empty dict...
            (['grep', 'p'],)                # ... or make a one-item tuple with a trailing comma
        ])
        # Result (on my Mac):
        Applications
    """
    first = True
    stdin = b""
    for argtuple in arg_kwarg_list:
        if len(argtuple) < 1:
            raise Exception("Found empty tuple")
        if len(argtuple) > 2:
            raise Exception(f"Found tuple with {len(argtuple)} elements")
        command = argtuple[0]
        kwargs = argtuple[1] if len(argtuple) == 2 else {}
        kwargs['stdout'] = subprocess.PIPE
        kwargs['stderr'] = subprocess.PIPE

        if not first:
            kwargs['stdin'] = subprocess.PIPE
        first = False

        process = subprocess.Popen(command, **kwargs)
        stdout, stderr = process.communicate(input=stdin)

        # Don't log stdin/stdout because it may contain binary
        logger.debug(
            f"Popen call {command} with keyword arguments {kwargs} "
            f"exited with code {process.returncode} "
            # f"with a stdin of '{stdin}' "
            # f"and with a stdout of '{stdout}' "
            f"and with a stderr of '{stderr.decode()}'"
        )
        if process.returncode != 0:
            raise subprocess.CalledProcessError(
                process.returncode, command, output=stdout, stderr=stderr)

        stdin = stdout

    return stdout


def resolvepath(path):
    return os.path.realpath(os.path.normpath(os.path.expanduser(path)))


def which(commandname):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)
    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        exe_file = os.path.join(path, commandname)
        if is_exe(exe_file):
            logger.info("Exe path is {}".format(exe_file))
            return exe_file
    logger.error("Could not find {} in PATH, which includes:\n{}".format(
        commandname, "\n".join(os.environ["PATH"].split(os.pathsep))))
    raise Exception("No such command '{}' in PATH".format(commandname))


# Example implementation functions

# You would obviously do this in a more pythonic way
# Meant to show an example of subprocess.call()
def echo(message):
    logger.debug("Asked to echo a message: {}".format(message))
    result = subprocess.call(['/bin/echo', message])
    return result


# You would obviously do this in a more pythonic way
# Meant to show an example of the subprocess.run()
def getuser():
    me = subprocess.run(['logname'], stdout=subprocess.PIPE).stdout.decode().rstrip()
    return me


# You would obviously do this in a more pythonic way
# Meant to show an example of the pipe function
def getpasshash():
    pwhash = pipe([
        (['grep', '^root:', 'passwd'], {'cwd': '/etc'}),
        (['cut', '-d', ':', '-f', '2'], {})
    ]).decode().rstrip()
    return pwhash


def whatever(something):
    logger.debug("I'm doing whatever with {}".format(something))
    print("yeah ok whatever, {}!!".format(something))


# Main handling
# The main() function is not special - it's invoked explicitly at the end
def main(*args, **kwargs):
    parser = argparse.ArgumentParser(
        description="A template for writing a new Python3 command line tool")
    parser.add_argument(
        "-d", action='store_true', dest='debug',
        help="Include debugging output")
    parser.add_argument(
        "directobject",
        default="mom", nargs='?',
        help="The object the command is working on")
    parsed = parser.parse_args()
    if parsed.debug:
        sys.excepthook = idb_excepthook
        logger.setLevel(logging.DEBUG)
    whatever(parsed.directobject)
    echo("message not interpreted by shell; this should be just the raw string $HOME, not my homedir")
    echo(f"My username is: {getuser()}")
    print(f"root's password hash is: {getpasshash()}")


# Unless we are running this script directly on the commandline, the main()
# function will NOT execute
if __name__ == '__main__':
    sys.exit(main(*sys.argv))
