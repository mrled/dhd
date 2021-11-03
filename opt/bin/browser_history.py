#!/usr/bin/env python3


import argparse
import collections
import dataclasses
import datetime
import logging
import os
import pdb
import shutil
import sqlite3
import subprocess
import sys
import tempfile
import traceback
import typing


logging.basicConfig(
    level=logging.INFO, format="[%(asctime)s] [%(name)s] [%(levelname)s] %(message)s"
)
logger = logging.getLogger(__name__)


def idb_excepthook(type, value, tb):
    """Call an interactive debugger in post-mortem mode

    If you do "sys.excepthook = idb_excepthook", then an interactive debugger
    will be spawned at an unhandled exception
    """
    if hasattr(sys, "ps1") or not sys.stderr.isatty():
        sys.__excepthook__(type, value, tb)
    else:
        traceback.print_exception(type, value, tb)
        print
        pdb.pm()


def resolvepath(path):
    return os.path.realpath(os.path.normpath(os.path.expanduser(path)))


@dataclasses.dataclass
class FirefoxHistoryEntry:
    url: str
    title: str
    last_visited: datetime.datetime

    def __str__(self) -> str:
        return f"{self.url} ({self.title}) @{self.last_visited}"


@dataclasses.dataclass
class FirefoxBookmarkEntry:
    url: str
    title: str
    bookmarked: datetime.datetime

    def __str__(self) -> str:
        return f"{self.url} ({self.title}) @{self.bookmarked}"


@dataclasses.dataclass
class FirefoxProfileUrls:
    profile: str
    history: typing.List[FirefoxHistoryEntry]
    bookmarks: typing.List[FirefoxBookmarkEntry]


"""
TODO: Add support for other browsers

Safari:
    default="~/Library/Safari/History.db"
    cp "$default" "$OUTPUT_DIR/safari_history.db.tmp"

    sqlite3 "$OUTPUT_DIR/safari_history.db.tmp" "select url from history_items" > "$OUTPUT_DIR/safari_history.json"

Chrome:
    default=$(ls ~/Library/Application\ Support/Google/Chrome/Default/History)
    cp "$default" "$OUTPUT_DIR/chrome_history.db.tmp"

    sqlite3 "$OUTPUT_DIR/chrome_history.db.tmp" "SELECT \"[\" || group_concat(json_object('timestamp', last_visit_time, 'description', title, 'href', url)) || \"]\" FROM urls;" > "$OUTPUT_DIR/chrome_history.json"
    jq < "$(dirname "${2:-$default}")"/Bookmarks '.roots.other.children[] | {href: .url, description: .name, timestamp: .date_added}' > "$OUTPUT_DIR/chrome_bookmarks.json"
"""


def archivebox_ff_profiles(newerthan=None) -> typing.List[FirefoxProfileUrls]:
    """Run archivebox for all firefox profiles (assumes macOS)"""
    ff_profiles_parent = resolvepath("~/Library/Application Support/Firefox/Profiles")

    result = []

    # Apparently Python cannot connect with "?immutable=1" for some reason?
    # which would let us connect even if the db is locked (b/c ff is open).
    # So we cheat and just copy the database. lol
    with tempfile.TemporaryDirectory() as tmpdir:

        for child in os.listdir(ff_profiles_parent):
            resolvedchild = os.path.join(ff_profiles_parent, child)
            if not os.path.isdir(resolvedchild):
                continue
            places = os.path.join(resolvedchild, "places.sqlite")
            if not os.path.exists(places):
                continue

            places2 = os.path.join(tmpdir, "places2.sqlite")
            shutil.copy(places, places2)

            # Open the database in immutable mode
            # Allows connecting even if the database is locked
            # (and therefore even when Firefox is running)
            con = sqlite3.connect(places2)
            con.row_factory = sqlite3.Row

            cur = con.cursor()
            history_sql = "SELECT datetime(last_visit_date/1000000, 'unixepoch') as parsedlvd, title, url FROM moz_places"
            if newerthan:
                history_sql += f" WHERE parsedlvd > '{newerthan}'"

            history_result = cur.execute(history_sql)

            history = [
                FirefoxHistoryEntry(h["url"], h["title"], h["parsedlvd"])
                for h in history_result
            ]
            logger.info(f"{resolvedchild} profile has {len(history)} history items")
            # for h in history:
            #     print(h)

            # history_strs = [
            #     f"{h['url']} ({h['title']}) @{h['parsedlvd']}" for h in history
            # ]
            # history_input = "\n".join(history_strs[0:10]).encode()
            # subprocess.run(["archivebox", "add"], input=history_input)

            logger.info(f"Bookmarks for profile {resolvedchild}:")
            bookmarks_result = cur.execute(
                "SELECT b.dateAdded, b.title, f.url FROM moz_bookmarks as b JOIN moz_places AS f ON f.id = b.fk;"
            )
            bookmarks = [
                FirefoxBookmarkEntry(b["url"], b["title"], b["dateAdded"])
                for b in bookmarks_result
            ]
            logger.debug(f"{resolvedchild} profile has {len(bookmarks)} bookmarks")
            # for b in bookmarks:
            #     print(f"{b['url']} ({b['title']}) @{b['dateAdded']}")

            result += [FirefoxProfileUrls(resolvedchild, history, bookmarks)]

    return result

    # subprocess.run(['archivebox', 'add'], input=history)
    # subprocess.run(['archivebox', 'add'], input=bookmarks)


def show_profiles_urls(profiles, bookmarks=False, history=False):
    """Given a list of FirefoxProfileUrls objects, show the URLs"""
    for profile in profiles:
        print(f"Profile {profile.profile}")
        if history:
            print("History")
            for entry in profile.history:
                print(entry)
        if bookmarks:
            print("Bookmarks")
            for entry in profile.bookmarks:
                print(entry)


def urlentries2domains(entries):
    """Given a list of history or bookmarks entries, return a list of just the domains"""
    # Sometimes an incomplete URL gets added to history. A real example:
    # place:type=6&sort=14&maxResults=10
    # Also, bookmarklets might do this I guess
    # Just ignore it.
    domains = [
        e.url.split("/")[2]
        for e in entries
        if e.url.startswith("http://") or e.url.startswith("https://")
    ]
    return domains


def show_sorted_uniq_count_items(items: typing.List[str]):
    """Given a list of items, print a uniq'd version of it with each item prefixed by its count"""
    collectionctr = collections.Counter(items).most_common()
    sorted_collectionctr = sorted(collectionctr, key=lambda d: d[1])
    largest_count = sorted_collectionctr[-1][1]
    for item, count in sorted_collectionctr:
        print(f"{str(count).ljust(len(str(largest_count)), ' ')} {item}")


def domainstats_profiles_urls(
    profiles: typing.List[FirefoxProfileUrls], bookmarks=False, history=False
):
    """Given a list of FirefoxProfileUrls objects, show domain stats"""
    for profile in profiles:
        print(f"Profile {profile.profile}")
        if history:
            print("History")
            show_sorted_uniq_count_items(urlentries2domains(profile.history))
        if bookmarks:
            print("Bookmarks")
            show_sorted_uniq_count_items(urlentries2domains(profile.bookmarks))
    return


def parseargs(arguments: typing.List[str]):
    """Parse program arguments"""
    parser = argparse.ArgumentParser(
        description="Get browser history. Currently only Firefox on macOS is supported."
    )
    parser.add_argument(
        "--debug",
        "-d",
        action="store_true",
        help="Launch a debugger on unhandled exception",
    )
    parser.add_argument(
        "--newer-than",
        default="",
        help="Only look at Firefox history newer than this date. Could slowly move this back as more and more history gets archived with time.",
    )
    parser.add_argument(
        "--urls",
        default="all",
        choices=["bookmarks", "history", "all"],
        help="Which URLs to return, either 'bookmarks' or 'history' or 'all'.",
    )
    parser.add_argument(
        "--process",
        default="show",
        choices=["show", "domainstats"],
        help="How to process the result. Show all with 'show', calculate domain stats with 'domainstats'.",
    )
    parsed = parser.parse_args(arguments)
    return parsed


def main(*arguments):
    """Main program"""
    parsed = parseargs(arguments[1:])
    if parsed.debug:
        sys.excepthook = idb_excepthook

    profiles = archivebox_ff_profiles(newerthan=parsed.newer_than)
    urls_bookmarks = parsed.urls in ["bookmarks", "all"]
    urls_history = parsed.urls in ["history", "all"]
    if parsed.process == "show":
        show_profiles_urls(profiles, urls_bookmarks, urls_history)
    elif parsed.process == "domainstats":
        domainstats_profiles_urls(profiles, urls_bookmarks, urls_history)


if __name__ == "__main__":
    sys.exit(main(*sys.argv))
