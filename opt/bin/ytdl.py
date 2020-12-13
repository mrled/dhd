#!/usr/bin/env python3

import argparse
import json
import logging
import plistlib
import subprocess
import sys

import youtube_dl


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


def download(uri, download_archive, maxfilenamelen=None, verbose=False):
    """Wrap the youtube-dl program
    """
    with youtube_dl.YoutubeDL({
        'verbose': True,

        'writedescription': True,
        'writeinfojson': True,
        'writeannotations': True,
        'write_all_thumbnails': True,
        'writesubtitles': True,
        'writeautomaticsub': True,
        'allsubtitles': True,

        'download_archive': download_archive,
        'nooverwrites': True,
    }) as ytdl:
        result = ytdl.download([uri])


def downloaduris(uris, archive, faillist):
    idx = 1
    for uri in uris:
        LOGGER.debug(f"Looping through URIs, processing {idx}/{len(uris)}: {uri}")
        idx += 1
        if uri == 'about:blank':
            LOGGER.debug("Skipping about:blank...")
            continue
        try:
            download(uri, archive)
        #except subprocess.CalledProcessError:
        except BaseException as exc:
            LOGGER.warning(f"Error attempting to download {uri}: ({type(exc)}) {exc}")
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


def parse_icab_tabs(tabsfile):
    """Parse a plist of tabs generated from iCab Mobile

    Return a list of every URI in the file

    Tabs plists contain a single list of tabs.
    Each tab is a dict with 'title' and 'url' keys.
    """
    with open(tabsfile, 'rb') as fp:
        parsed = plistlib.load(fp)
    return [p['url'] for p in parsed]


def main(*args, **kwargs):
    parser = argparse.ArgumentParser(description="A youtube-dl wrapper")
    parser.add_argument("--debug", "-d", action="store_true", help="Show debug messages")
    parser.add_argument("--txt", help="Parse a txt file containing one URL per line")
    parser.add_argument("--html", help="Use an XML/HTML file containing URLs")
    parser.add_argument("--icab-tabs", help="Parse a Plist file from iCab containing URLs of open tabs")
    parser.add_argument("--archive", "-a", required=True, help="The download archive, which youtube-dl uses to record successful downloads (requied)")
    parser.add_argument("uri", nargs="*", help="URI to download")
    parsed = parser.parse_args()
    if parsed.debug:
        sys.excepthook = idb_excepthook
        LOGGER.setLevel(logging.DEBUG)
    if parsed.txt:
        downloaduris(parsetxt(parsed.txt), parsed.archive, f'{parsed.txt}.failedurls.txt')
    if parsed.html:
        downloaduris(parsehtml(parsed.html), parsed.archive, f'{parsed.html}.failedurls.txt')
    if parsed.icab_tabs:
        downloaduris(parse_icab_tabs(parsed.icab_tabs), parsed.archive, f'{parsed.icab_tabs}.failedurls.txt')
    if parsed.uri:
        downloaduris(parsed.uri, 'cmdline.failedurls.txt')


if __name__ == '__main__':
    sys.exit(main(*sys.argv))


