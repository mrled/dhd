#!/usr/bin/env python3

import argparse
import sys


def strace():
    import pdb
    pdb.set_trace()


def whatever(something):
    print("yeah ok whatever, {}!!".format(something))


def main(*args, **kwargs):
    parser = argparse.ArgumentParser(
        description="A template for writing a new Python3 command line tool")
    parser.add_argument(
        "directobject",
        default="mom", nargs='?',
        help="The object the command is working on")
    parsed = parser.parse_args()
    whatever(parsed.directobject)


if __name__ == '__main__':
    sys.exit(main(*sys.argv))
