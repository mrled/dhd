#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys


scriptdir = os.path.dirname(os.path.realpath(__file__))
debug = False


def strace():
    import pdb
    pdb.set_trace()


def debugprint(message):
    global debug
    if debug:
        print(message)


def echo(message):
    result = subprocess.call(['/bin/echo', message])
    return result


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
