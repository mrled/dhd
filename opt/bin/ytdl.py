#!/usr/bin/env python3

import argparse
import json
import logging
import subprocess
import sys


logging.basicConfig(level=logging.INFO, format='[%(asctime)s] [%(name)s] [%(levelname)s] %(message)s')
LOGGER = logging.getLogger(__name__)


def idb_excepthook(type, value, tb):
    """Call an interactive debugger in post-mortem mode

    If you do "sys.excepthook = idb_excepthook", then an interactive debugger
    will be spawned at an unhandled exception
    """
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        sys.__excepthook__(type, value, tb)
    else:
        import pdb, traceback
        traceback.print_exception(type, value, tb)
        print
        pdb.pm()


def download(uri, maxfilenamelen=92, verbose=False):
    """Wrap the youtube-dl program
    """
    LOGGER.info(f"Downloading from URI: {uri}")
    djproc = subprocess.run(['youtube-dl', '--dump-json', uri], check=True, stdout=subprocess.PIPE)
    loaded = json.loads(djproc.stdout)
    LOGGER.debug(json.dumps(loaded, indent=2, sort_keys=True))
    filename = loaded['_filename'][0:maxfilenamelen]
    if len(filename) < len(loaded['_filename']):
        extension = loaded['_filename'].split('.')[-1]
        if extension == loaded['_filename']:
            extension = ""
        filename = filename + extension
    jsonfilename = f"{filename}.youtube-dl.json"
    with open(jsonfilename, 'w') as jsf:
        jsf.write(djproc.stdout.decode())
    dlproc = subprocess.run(['youtube-dl', '--verbose', '-o', filename, uri], check=True)


def downloaduris(uris, faillist):
    for uri in uris:
        LOGGER.debug(f"Looping through URIs, processing {uri}")
        try:
            download(uri)
        except subprocess.CalledProcessError:
            with open(faillist, 'a') as flf:
                flf.write(uri + "\n")


def parsetxt(txtfile):
    """Parse a text file with one URI per line

    Return a list of URIs
    """
    with open(txtfile) as tf:
        return tf.readlines()


def parsehtml(htmlfile):
    """Parse an HTML file containing URIs

    Return a list of every URI in the file
    """
    ldproc = subprocess.run(
        ['lynx', '-dump', '-listonly', '-nonumbers', htmlfile], stdout=subprocess.PIPE, check=True)
    return ldproc.stdout.split("\n")


def main(*args, **kwargs):
    parser = argparse.ArgumentParser(description="A youtube-dl wrapper")
    parser.add_argument("--debug", "-d", action="store_true", help="Show debug messages")
    parser.add_argument("--txt", "-t", help="Parse a txt file containing one URL per line")
    parser.add_argument("--html", "-x", help="Use an XML/HTML file containing URLs")
    parser.add_argument("uri", nargs="*", help="URI to download")
    parsed = parser.parse_args()
    if parsed.debug:
        sys.excepthook = idb_excepthook
        LOGGER.setLevel(logging.DEBUG)
    if parsed.txt:
        downloaduris(parsetxt(parsed.txt), f'{parsed.txt}.failedurls.txt')
    if parsed.html:
        downloaduris(parsehtml(parsed.html), f'{parsed.html}.failedurls.txt')
    if parsed.uri:
        downloaduris(parsed.uri, 'ytdl.stdin.failed')


if __name__ == '__main__':
    sys.exit(main(*sys.argv))


