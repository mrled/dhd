#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys


scriptdir = os.path.dirname(os.path.realpath(__file__))
debug = False


# Helper functions


def strace():
    import pdb
    pdb.set_trace()


def debugprint(message):
    global debug
    if debug:
        print(message)


def resolvepath(path):
    return os.path.realpath(os.path.normpath(os.path.expanduser(path)))


def which(commandname):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)
    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        exe_file = os.path.join(path, commandname)
        if is_exe(exe_file):
            return exe_file
    print("Exe path is {}".format(exe_file))
    raise Exception("No such command '{}' in %PATH%".format(commandname))


# Example functions


def echo(message):
    result = subprocess.call(['/bin/echo', message])
    return result


# Implementation functions


def whatever(something):
    print("yeah ok whatever, {}!!".format(something))


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
        global debug
        debug = True
    whatever(parsed.directobject)


if __name__ == '__main__':
    sys.exit(main(*sys.argv))
