#!/usr/bin/env python3

import argparse
import logging
import os
import subprocess
import sys


scriptdir = os.path.dirname(os.path.realpath(__file__))


# Helper functions

def strace():
    import pdb
    pdb.set_trace()


def resolvepath(path):
    return os.path.realpath(os.path.normpath(os.path.expanduser(path)))


def which(commandname):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)
    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        exe_file = os.path.join(path, commandname)
        if is_exe(exe_file):
            logging.info("Exe path is {}".format(exe_file))
            return exe_file
    logging.error("Could not find {} in PATH, which includes:\n{}".format(
        commandname, "\n".join(os.environ["PATH"].split(os.pathsep))))
    raise Exception("No such command '{}' in PATH".format(commandname))


# Example implementation functions

def echo(message):
    logging.debug("Asked to echo a message: {}".format(message))
    result = subprocess.call(['/bin/echo', message])
    return result


def whatever(something):
    logging.debug("I'm doing whatever with {}".format(something))
    print("yeah ok whatever, {}!!".format(something))


# Main handling
# The main() function is not special - it's invoked explicitly at the end
def main(*args, **kwargs):
    logging.basicConfig()
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
        logging.root.setLevel(logging.DEBUG)
    whatever(parsed.directobject)


# Unless we are running this script directly on the commandline, the main()
# function will NOT execute
if __name__ == '__main__':
    sys.exit(main(*sys.argv))
