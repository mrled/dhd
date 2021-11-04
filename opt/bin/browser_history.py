#!/usr/bin/env python3


import argparse
import collections
import collections.abc
import dataclasses
import datetime
import json
import logging
import os
import pdb
import plistlib
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
class BrowserHistoryEntry:
    url: str
    title: str
    last_visited: datetime.datetime

    def __str__(self) -> str:
        return f"{self.url} ({self.title}) @{self.last_visited}"


@dataclasses.dataclass
class BrowserBookmarkEntry:
    url: str
    title: str
    bookmarked: typing.Optional[datetime.datetime]

    def __str__(self) -> str:
        return f"{self.url} ({self.title}) @{self.bookmarked}"


@dataclasses.dataclass
class BrowserProfile:
    profile: str
    history: typing.List[BrowserHistoryEntry]
    bookmarks: typing.List[BrowserBookmarkEntry]


'''
TODO: Add support for other browsers

Safari:
    default="~/Library/Safari/History.db"
    cp "$default" "$OUTPUT_DIR/safari_history.db.tmp"

    sqlite3 "$OUTPUT_DIR/safari_history.db.tmp" "select url from history_items" > "$OUTPUT_DIR/safari_history.json"

                _SQL = """SELECT url, title, datetime(visit_time + 978307200, 'unixepoch', 'localtime')
                                    FROM history_visits INNER JOIN history_items ON history_items.id = history_visits.history_item ORDER BY visit_time DESC"""

Chrome:
    default=$(ls ~/Library/Application\ Support/Google/Chrome/Default/History)
    cp "$default" "$OUTPUT_DIR/chrome_history.db.tmp"

    sqlite3 "$OUTPUT_DIR/chrome_history.db.tmp" "SELECT \"[\" || group_concat(json_object('timestamp', last_visit_time, 'description', title, 'href', url)) || \"]\" FROM urls;" > "$OUTPUT_DIR/chrome_history.json"
'''


def get_safari_urls(newerthan=None) -> typing.List[BrowserProfile]:
    """Get URLs from Safari profiles

    Actually I think Safari just has one profile, but still return a list so this works the same as for other browsers
    """
    profile_dir = resolvepath("~/Library/Safari")
    with tempfile.TemporaryDirectory() as tmpdir:
        history = []
        histfile = os.path.join(profile_dir, "History.db")
        if os.path.exists(histfile):
            histfile2 = os.path.join(tmpdir, "Safari.History.db")
            shutil.copy(histfile, histfile2)

            con = sqlite3.connect(histfile2)
            con.row_factory = sqlite3.Row

            cur = con.cursor()

            history_sql = f"SELECT url, title, datetime(visit_time + 978307200, 'unixepoch', 'localtime') as parseddate FROM history_visits  INNER JOIN history_items ON history_items.id = history_visits.history_item"
            if newerthan:
                history_sql += f" WHERE parseddate > '{newerthan}'"
            history_result = cur.execute(history_sql)
            history = [
                BrowserHistoryEntry(h["url"], h["title"], h["parseddate"])
                for h in history_result
            ]
            logger.info(f"{profile_dir} profile has {len(history)} history items")

        bookmarks = []
        marksfile = os.path.join(profile_dir, "Bookmarks.plist")
        if os.path.exists(marksfile):
            with open(marksfile, "rb") as fp:
                marksdata = plistlib.load(fp)
            bookmarks = [
                BrowserBookmarkEntry(i["URLString"], i["URIDictionary"]["title"], None)
                for i in marksdata["Children"]
                if i["WebBookmarkType"] == "WebBookmarkTypeLeaf"
            ]

    profile = BrowserProfile(profile_dir, history, bookmarks)
    return [profile]


def chrome_bookmarks_extractor(d: dict) -> typing.List[BrowserBookmarkEntry]:
    """Given a dict from reading a Chrome bookmarks file as JSON, return a list of all bookmarks.

    The initial dictionary passed should be bookmark_json_data['roots'].

    Properly handle nested bookmark folders.
    """
    if "roots" in d:
        bar_bookmarks = chrome_bookmarks_extractor(d["roots"]["bookmark_bar"])
        other_bookmarks = chrome_bookmarks_extractor(d["roots"]["other"])

        # Instead of flattening this, I should write a good recursive function that does not need flattening.
        # Sad!
        flatten = lambda l: sum(map(flatten, l), []) if isinstance(l, list) else [l]

        all_bookmarks = flatten(bar_bookmarks + other_bookmarks)

        return all_bookmarks
    if "children" in d:
        return [chrome_bookmarks_extractor(c) for c in d["children"]]
    return BrowserBookmarkEntry(d["url"], d["name"], d["date_added"])


def get_chrome_urls(newerthan=None) -> typing.List[BrowserProfile]:
    """Get URLs from all Chrome profiles"""
    profiles_parent = resolvepath("~/Library/Application Support/Google/Chrome")
    result = []
    with tempfile.TemporaryDirectory() as tmpdir:

        for child in os.listdir(profiles_parent):
            if not (child == "Default" or child.startswith("Profile")):
                continue
            resolvedchild = os.path.join(profiles_parent, child)
            if not os.path.isdir(resolvedchild):
                continue

            history = []
            histfile = os.path.join(resolvedchild, "History")
            if os.path.exists(histfile):
                histfile2 = os.path.join(tmpdir, "history.sqlite")
                shutil.copy(histfile, histfile2)

                con = sqlite3.connect(histfile2)
                con.row_factory = sqlite3.Row

                cur = con.cursor()
                history_sql = "SELECT datetime(last_visit_time/1000000, 'unixepoch') as parseddate, title, url FROM urls"
                if newerthan:
                    history_sql += f" WHERE parseddate > '{newerthan}'"
                history_result = cur.execute(history_sql)
                history = [
                    BrowserHistoryEntry(h["url"], h["title"], h["parseddate"])
                    for h in history_result
                ]
                logger.info(f"{resolvedchild} profile has {len(history)} history items")

            bookmarks = []
            bookmarksfile = os.path.join(resolvedchild, "Bookmarks")
            if os.path.exists(bookmarksfile):
                logger.info(f"Bookmarks for profile {resolvedchild}:")
                with open(bookmarksfile) as bkmkfp:
                    bookmarks_data = json.load(bkmkfp)
                bookmarks = chrome_bookmarks_extractor(bookmarks_data)

            result += [BrowserProfile(resolvedchild, history, bookmarks)]

    return result


def get_firefox_urls(newerthan=None) -> typing.List[BrowserProfile]:
    """Get URLs from all Firefox profiles"""
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

            con = sqlite3.connect(places2)
            con.row_factory = sqlite3.Row

            cur = con.cursor()
            history_sql = "SELECT datetime(last_visit_date/1000000, 'unixepoch') as parsedlvd, title, url FROM moz_places"
            if newerthan:
                history_sql += f" WHERE parsedlvd > '{newerthan}'"

            history_result = cur.execute(history_sql)

            history = [
                BrowserHistoryEntry(h["url"], h["title"], h["parsedlvd"])
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
                BrowserBookmarkEntry(b["url"], b["title"], b["dateAdded"])
                for b in bookmarks_result
            ]
            logger.debug(f"{resolvedchild} profile has {len(bookmarks)} bookmarks")
            # for b in bookmarks:
            #     print(f"{b['url']} ({b['title']}) @{b['dateAdded']}")

            result += [BrowserProfile(resolvedchild, history, bookmarks)]

    return result


def show_profiles_urls_metadata(profiles, bookmarks=False, history=False):
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


def show_profiles_urls(profiles, bookmarks=False, history=False):
    """Given a list of FirefoxProfileUrls objects, show the URLs"""
    for profile in profiles:
        if history:
            for entry in profile.history:
                print(entry.url)
        if bookmarks:
            for entry in profile.bookmarks:
                print(entry.url)


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
    profiles: typing.List[BrowserProfile], bookmarks=False, history=False
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
        "--browsers",
        default="all",
        choices=["firefox", "chrome", "safari", "all"],
        help="Which browsers to get history from",
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
        default="urls-metadata",
        choices=["urls-metadata", "urls", "domainstats"],
        help="How to process the result. Show URL/title/date with 'urls-metadata', show just URLs with 'urls', calculate domain stats with 'domainstats'.",
    )
    parsed = parser.parse_args(arguments)
    return parsed


# A function that works like a typical Unix main() function
MainFunction = collections.abc.Callable[[typing.List[str], int]]


def main(*arguments: typing.List[str]) -> int:
    """Main program"""
    parsed = parseargs(arguments[1:])
    if parsed.debug:
        sys.excepthook = idb_excepthook
        logger.setLevel(logging.DEBUG)

    profiles = []

    urls_bookmarks = parsed.urls in ["bookmarks", "all"]
    urls_history = parsed.urls in ["history", "all"]

    if parsed.browsers in ["firefox", "all"]:
        profiles += get_firefox_urls(newerthan=parsed.newer_than)
    if parsed.browsers in ["chrome", "all"]:
        profiles += get_chrome_urls(newerthan=parsed.newer_than)
    if parsed.browsers in ["safari", "all"]:
        profiles += get_safari_urls(newerthan=parsed.newer_than)

    if parsed.process == "urls-metadata":
        show_profiles_urls_metadata(profiles, urls_bookmarks, urls_history)
    elif parsed.process == "urls":
        show_profiles_urls(profiles, urls_bookmarks, urls_history)
    elif parsed.process == "domainstats":
        domainstats_profiles_urls(profiles, urls_bookmarks, urls_history)
    else:
        raise Exception(f"Unknown value for --process: {parsed.process}")

    return 0


def broken_pipe_handler(func: MainFunction, *arguments: typing.List[str]) -> int:
    """Handler for broken pipes

    Wrap the main() function in this to properly handle broken pipes
    without a giant nastsy backtrace.

    See <https://docs.python.org/3/library/signal.html#note-on-sigpipe>
    """
    try:
        returncode = func(*arguments)
        sys.stdout.flush()
    except BrokenPipeError:
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, sys.stdout.fileno())
        # Convention is 128 + whatever the return code would otherwise be
        returncode = 128 + 1
    return returncode


if __name__ == "__main__":
    exitcode = broken_pipe_handler(main, *sys.argv)
    sys.exit(exitcode)
