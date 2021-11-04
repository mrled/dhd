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


MIN_PYTHON = (3, 9)
if sys.version_info < MIN_PYTHON:
    sys.exit("Python %s.%s or later is required.\n" % MIN_PYTHON)


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
    browser: str
    profile: str
    history: typing.List[BrowserHistoryEntry]
    bookmarks: typing.List[BrowserBookmarkEntry]


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

    profile = BrowserProfile("Safari", profile_dir, history, bookmarks)
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

            result += [BrowserProfile("Chrome", resolvedchild, history, bookmarks)]

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

            result += [BrowserProfile("Firefox", resolvedchild, history, bookmarks)]

    return result


def profiles_html(
    profiles: typing.List[BrowserProfile],
    bookmarks: bool = False,
    history: bool = False,
):
    """Given a list of BrowserProfile objects, return an HTML report."""
    result = []
    result += ["<h1>Browser profiles</h1>"]
    for profile in profiles:
        result += [f"<h2>{profile.browser} profile at {profile.profile}</h2>"]
        if history:
            result += [f"<h3>History</h3>"]
            result += ["<ul>"]
            for entry in profile.history:
                result += [
                    f'<li><a href="{entry.url}">{entry.title} @{entry.last_visited}</a></li>'
                ]
            result += ["</ul>"]
        if bookmarks:
            result += [f"<h3>Bookmarks</h3>"]
            result += ["<ul>"]
            for entry in profile.bookmarks:
                result += [
                    f'<li><a href="{entry.url}">{entry.title} @{entry.bookmarked}</a></li>'
                ]
            result += ["</ul>"]
    return "\n".join(result)


def profiles_markdown(
    profiles: typing.List[BrowserProfile],
    bookmarks: bool = False,
    history: bool = False,
):
    """Given a list of BrowserProfile objects, return a Markdown report."""
    result = []
    result += ["# Browser profiles", ""]
    for profile in profiles:
        result += [f"## {profile.browser} profile at {profile.profile}", ""]
        if history:
            result += [f"### History", ""]
            for entry in profile.history:
                result += [f"- [{entry.title @{entry.last_visited}}]({entry.url})"]
            result += [""]
        if bookmarks:
            result += ["### Bookmarks", ""]
            for entry in profile.bookmarks:
                result += [f"- [{entry.title} @{entry.bookmarked}]({entry.url})"]
            result += [""]
    return "\n".join(result)


def profiles_urls_txt(
    profiles: typing.List[BrowserProfile],
    bookmarks: bool = False,
    history: bool = False,
):
    """Given a list of BrowserProfile objects, return a list of URLs."""
    result = []
    for profile in profiles:
        hurls = []
        if history:
            hurls = [entry.url for entry in profile.history]
        burls = []
        if bookmarks:
            burls += [entry.url for entry in profile.bookmarks]
        result += hurls + burls
    return "\n".join(result)


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
    result = []
    for item, count in sorted_collectionctr:
        result += [f"{str(count).ljust(len(str(largest_count)), ' ')} {item}"]
    return result


def profiles_urls_domainstats(
    profiles: typing.List[BrowserProfile],
    bookmarks: bool = False,
    history: bool = False,
):
    """Given a list of BrowserProfile objects, show domain stats"""
    result = []
    for profile in profiles:
        result += [f"Profile {profile.profile}"]
        if history:
            result += ["History"]
            if profile.history:
                result += show_sorted_uniq_count_items(
                    urlentries2domains(profile.history)
                )
        if bookmarks:
            result += ["Bookmarks"]
            if profile.bookmarks:
                result += show_sorted_uniq_count_items(
                    urlentries2domains(profile.bookmarks)
                )
    return "\n".join(result)


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
        "--history-newer-than",
        help="Only look at browser history newer than this date. Has no effect on bookmarks. Could slowly move this back as more and more history gets archived with time.",
    )
    parser.add_argument(
        "--history",
        default=True,
        action=argparse.BooleanOptionalAction,
        help="Return history (default True)",
    )
    parser.add_argument(
        "--bookmarks",
        default=True,
        action=argparse.BooleanOptionalAction,
        help="Return bookmarks (default True)",
    )
    parser.add_argument(
        "--format",
        default="html",
        choices=["html", "markdown", "urls", "domainstats"],
        help="The output format. html: A list with <a href='http://...'>Title @ date</a>. markdown: A list with [Title @ date](http://...) urls: Just the URLs, one per line. domainstats: Calculate number of times each domain is present (calculated separately per profile).",
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

    if parsed.browsers in ["firefox", "all"]:
        profiles += get_firefox_urls(newerthan=parsed.history_newer_than)
    if parsed.browsers in ["chrome", "all"]:
        profiles += get_chrome_urls(newerthan=parsed.history_newer_than)
    if parsed.browsers in ["safari", "all"]:
        profiles += get_safari_urls(newerthan=parsed.history_newer_than)

    if parsed.format == "html":
        print(profiles_html(profiles, parsed.bookmarks, parsed.history))
    elif parsed.format == "markdown":
        print(profiles_markdown(profiles, parsed.bookmarks, parsed.history))
    elif parsed.format == "urls":
        print(profiles_urls_txt(profiles, parsed.bookmarks, parsed.history))
    elif parsed.format == "domainstats":
        print(profiles_urls_domainstats(profiles, parsed.bookmarks, parsed.history))
    else:
        raise Exception(f"Unknown value for --format: {parsed.format}")

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
