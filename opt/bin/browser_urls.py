#!/usr/bin/env python3


import argparse
import collections
import collections.abc
import configparser
import dataclasses
import datetime
import json
import logging
import os
import pdb
import plistlib
import re
import shutil
import sqlite3
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


BrowserUrlContainer = typing.Union[BrowserBookmarkEntry, BrowserHistoryEntry]


@dataclasses.dataclass
class BrowserProfile:
    browser: str
    profile: str
    history: typing.List[BrowserHistoryEntry]
    bookmarks: typing.List[BrowserBookmarkEntry]


@dataclasses.dataclass
class ExpressionV:
    """An expression that matches URLs to be omitted, like 'grep -v'"""

    omit: re.Pattern

    def __str__(self) -> str:
        return f"v:{self.omit.pattern}"

    def apply(self, url) -> str:
        if self.omit.search(url):
            logging.debug(f"Omitting {url}")
            return None
        logging.debug(f"Keeping {url}")
        return url


@dataclasses.dataclass
class ExpressionS:
    """An expression that matches URLs to be modified, like 'sed s/.../.../'"""

    pattern: re.Pattern
    replace: str
    splitchar: str = "@"

    def apply(self, url: str) -> str:
        """Apply the expression to a URL"""
        newurl = self.pattern.sub(self.replace, url)
        logging.debug(f"Replacing {url} with {newurl}")
        return newurl

    def __str__(self) -> str:
        return f"s{self.splitchar}{self.pattern.pattern}{self.splitchar}{self.replace}"


Expression = typing.Union[ExpressionV, ExpressionS]


def parse_filters_config_file(configfile: str) -> list[Expression]:
    """Return a function that filters out URLs based on the config file

    Format example:

    v:^https?://(www\.)?kagi.com/
    s@^https?://(www\.)?reddit.com@https://old.reddit.com

    The 'v' lines (like 'grep -v') are omitted, and the 's' lines are modified.
    The 's' lines (like 'sed s/.../.../') are split on the first character after the 's';
    the first part is the pattern, and the second part is the replacement.
    """

    expressions: list[Expression] = []

    with open(configfile) as fp:
        for idx, line in enumerate(fp):
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if line.startswith("v:"):
                logging.debug(f"Adding omit expression {line}")
                expressions.append(ExpressionV(re.compile(line[2:])))
            elif line.startswith("s"):
                splitchar = line[1]
                splitchar_count = line.count(splitchar)
                if splitchar_count != 2:
                    raise Exception(
                        f"Bad config line #{idx}: {line} (expected 2 '{splitchar}' characters, got {splitchar_count})"
                    )
                _, patternstr, replace = line.split(splitchar)
                logging.debug(f"Adding replace expression {line}")
                expressions.append(
                    ExpressionS(re.compile(patternstr), replace, splitchar)
                )
            else:
                raise Exception(f"Unknown config line #{idx}: {line}")

    for exp in expressions:
        logging.info(f"Expression from config file: {exp}")

    return expressions


def filter_browser_urls_from_expressions(
    expressions: list[Expression], items: list[BrowserUrlContainer]
) -> list[str]:
    """Filter URLs based on a list of expressions"""
    filtered: list[BrowserUrlContainer] = []
    for item in items:
        omit = False
        for exp in expressions:
            item.url = exp.apply(item.url)
            if item.url is None:
                omit = True
                break
        if not omit:
            filtered.append(item)
    return filtered


# def filter_urls_from_expressions(
#     expressions: list[Expression], urls: list[str]
# ) -> list[str]:
#     """Filter URLs based on a list of expressions"""
#     for url in urls:
#         for exp in expressions:
#             url = exp.apply(url)
#             if url is None:
#                 break

#     for exp in expressions:
#         urls = [exp.apply(url) for url in urls]
#         urls = [url for url in urls if url is not None]
#     return urls


def discover_safari_profiles() -> typing.List[str]:
    """Find Safari profiles in the home directory

    (This will only ever discover a single profile, but we add this for completeness.)
    """
    profile_dir = resolvepath("~/Library/Safari")
    if os.path.exists(profile_dir):
        return [profile_dir]
    return []


def discover_chrome_profiles() -> typing.List[str]:
    """Find Chrome profiles in the home directory"""
    chrome_profiles_parent = resolvepath("~/Library/Application Support/Google/Chrome")

    result = []

    for child in os.listdir(chrome_profiles_parent):
        if not (child == "Default" or child.startswith("Profile")):
            continue
        resolvedchild = os.path.join(chrome_profiles_parent, child)
        if not os.path.isdir(resolvedchild):
            continue
        history = os.path.join(resolvedchild, "History")
        if not os.path.exists(history):
            continue
        result += [resolvedchild]
    return result


def discover_firefox_profiles() -> typing.List[str]:
    """Find Firefox profiles in the home directory"""
    ff_profiles_parent = resolvepath("~/Library/Application Support/Firefox/Profiles")

    result = []

    for child in os.listdir(ff_profiles_parent):
        resolvedchild = os.path.join(ff_profiles_parent, child)
        if not os.path.isdir(resolvedchild):
            continue
        places = os.path.join(resolvedchild, "places.sqlite")
        if not os.path.exists(places):
            continue
        result += [resolvedchild]
    return result


def get_safari_urls(
    profilepaths: list[str], newerthan=None
) -> typing.List[BrowserProfile]:
    """Get URLs from Safari profiles"""

    profilepath = resolvepath("~/Library/Safari")
    profiles = []
    for profilepath in profilepaths:
        with tempfile.TemporaryDirectory() as tmpdir:
            history = []
            histfile = os.path.join(profilepath, "History.db")
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
                logger.info(f"{profilepath} profile has {len(history)} history items")

            bookmarks = []
            marksfile = os.path.join(profilepath, "Bookmarks.plist")
            if os.path.exists(marksfile):
                with open(marksfile, "rb") as fp:
                    marksdata = plistlib.load(fp)
                bookmarks = [
                    BrowserBookmarkEntry(
                        i["URLString"], i["URIDictionary"]["title"], None
                    )
                    for i in marksdata["Children"]
                    if i["WebBookmarkType"] == "WebBookmarkTypeLeaf"
                ]

        profiles.append(BrowserProfile("Safari", profilepath, history, bookmarks))
    return profiles


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


def get_chrome_urls(
    profilepaths: list[str], newerthan=None
) -> typing.List[BrowserProfile]:
    """Get URLs from all Chrome profiles"""
    result = []
    with tempfile.TemporaryDirectory() as tmpdir:

        for profilepath in profilepaths:
            history = []
            histfile = os.path.join(profilepath, "History")
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
                logger.info(f"{profilepath} profile has {len(history)} history items")

            bookmarks = []
            bookmarksfile = os.path.join(profilepath, "Bookmarks")
            if os.path.exists(bookmarksfile):
                logger.info(f"Bookmarks for profile {profilepath}:")
                with open(bookmarksfile) as bkmkfp:
                    bookmarks_data = json.load(bkmkfp)
                bookmarks = chrome_bookmarks_extractor(bookmarks_data)

            result += [BrowserProfile("Chrome", profilepath, history, bookmarks)]

    return result


def get_firefox_urls(
    profilepaths: list[str], newerthan=None
) -> typing.List[BrowserProfile]:
    """Get URLs from all Firefox profiles"""

    result = []

    # Apparently Python cannot connect with "?immutable=1" for some reason?
    # which would let us connect even if the db is locked (b/c ff is open).
    # So we cheat and just copy the database. lol
    with tempfile.TemporaryDirectory() as tmpdir:

        for profilepath in profilepaths:
            places = os.path.join(profilepath, "places.sqlite")
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
            logger.info(f"{profilepath} profile has {len(history)} history items")

            logger.info(f"Bookmarks for profile {profilepath}:")
            bookmarks_result = cur.execute(
                "SELECT b.dateAdded, b.title, f.url FROM moz_bookmarks as b JOIN moz_places AS f ON f.id = b.fk;"
            )
            bookmarks = [
                BrowserBookmarkEntry(b["url"], b["title"], b["dateAdded"])
                for b in bookmarks_result
            ]
            logger.debug(f"{profilepath} profile has {len(bookmarks)} bookmarks")

            result += [BrowserProfile("Firefox", profilepath, history, bookmarks)]

    return result


def get_file_urls(filename: str) -> BrowserProfile:
    """Get URLS from a list of files.

    Return a BrowserProfile object with bookmarks only (no history).
    """
    bookmarks: list[BrowserBookmarkEntry] = []
    with open(filename) as fp:
        for line in fp.readlines():
            line = line.strip()
            if not line:
                continue
            if line.startswith("#"):
                continue
            bookmarks.append(BrowserBookmarkEntry(line, f"No title: {line}", None))
    return [BrowserProfile("File", filename, [], bookmarks)]


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
    dedup: bool = False,
):
    """Given a list of BrowserProfile objects, return a list of URLs.

    If dedup is True, don't show a URL more than once.
    """
    result = []
    for profile in profiles:
        hurls = []
        if history:
            hurls = [entry.url for entry in profile.history]
        burls = []
        if bookmarks:
            burls += [entry.url for entry in profile.bookmarks]
        result += hurls + burls
    if dedup:
        result = set(result)
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


@dataclasses.dataclass
class Cfg:
    ignore_firefoxes: list[str]
    ignore_chromes: list[str]
    urlfilters: list[Expression]


def parseconfig(configfile: str):
    """Parse the config file and return a Cfg object"""

    config = Cfg([], [], [])

    cfgfile = configparser.ConfigParser()
    cfgfile.read(configfile)

    for line in cfgfile.get("DEFAULT", "ignoreprofiles").split("\n"):
        line = line.strip()
        if not line:
            continue
        browser, igpro = line.split(" ", maxsplit=1)
        if browser == "firefox":
            config.ignore_firefoxes.append(igpro)
        elif browser == "chrome":
            config.ignore_chromes.append(igpro)
        else:
            raise ValueError(f"Unknown browser {browser}")

    for idx, line in enumerate(cfgfile.get("DEFAULT", "urlfilters").split("\n")):
        line = line.strip()
        if not line:
            continue
        if line.startswith("v:"):
            logging.debug(f"Adding omit expression {line}")
            config.urlfilters.append(ExpressionV(re.compile(line[2:])))
        elif line.startswith("s"):
            splitchar = line[1]
            splitchar_count = line.count(splitchar)
            if splitchar_count != 2:
                raise Exception(
                    f"Bad URL filter #{idx}: {line} (expected 2 '{splitchar}' characters, got {splitchar_count})"
                )
            _, patternstr, replace = line.split(splitchar)
            logging.debug(f"Adding replace expression {line}")
            config.urlfilters.append(
                ExpressionS(re.compile(patternstr), replace, splitchar)
            )
        else:
            raise ValueError(f"Unknown URL filter #{idx}: {line}")

    return config


def parseargs(arguments: typing.List[str]):
    """Parse program arguments"""
    parser = argparse.ArgumentParser(
        description="Get browser history. Currently only Firefox on macOS is supported."
    )
    parser.add_argument(
        "--debug",
        "-d",
        action="store_true",
        help="Launch a debugger on unhandled exception and log verbose output",
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Log verbose output"
    )
    cfgfile = os.path.join(os.path.expanduser("~"), ".browser_urls.conf")
    parser.add_argument(
        "--config",
        "-c",
        default=cfgfile,
        help=f"Config file to use. Default is {cfgfile}.",
    )
    subparsers = parser.add_subparsers(dest="subcmd", required=True)

    sub_profiles = subparsers.add_parser(name="profiles", help="List browser profiles")

    sub_urls = subparsers.add_parser(
        name="urls", help="List URLs from browser profiles"
    )

    browsers = ["firefox", "chrome", "safari", "file", "all"]
    sub_urls.add_argument(
        "--browsers",
        default="all",
        choices=browsers,
        help=f"Which browsers to get history from, one of {browsers}. Default is all. 'file' requires the --url-file option.",
    )
    sub_urls.add_argument(
        "--history-newer-than",
        help="Only look at browser history newer than this date. Has no effect on bookmarks. Could slowly move this back as more and more history gets archived with time.",
    )
    sub_urls.add_argument(
        "--history",
        default=True,
        action=argparse.BooleanOptionalAction,
        help="Return history (default True)",
    )
    sub_urls.add_argument(
        "--bookmarks",
        default=True,
        action=argparse.BooleanOptionalAction,
        help="Return bookmarks (default True)",
    )
    sub_urls.add_argument("--url-file", help="Read URLs from this file")
    sub_urls.add_argument(
        "--format",
        default="html",
        choices=["html", "markdown", "urls", "urls-dedup", "domainstats"],
        help="The output format. html: A list with <a href='http://...'>Title @ date</a>. markdown: A list with [Title @ date](http://...) urls: Just the URLs, one per line. urls-dedup: Like 'urls', but with duplicates removed. domainstats: Calculate number of times each domain is present (calculated separately per profile).",
    )

    parsed = parser.parse_args(arguments)
    if parsed.subcmd == "urls" and parsed.browsers == "file" and not parsed.url_file:
        parser.error("--browsers file requires --url-file")
    return parsed


# A function that works like a typical Unix main() function
MainFunction = collections.abc.Callable[[typing.List[str], int]]


def main(*arguments: typing.List[str]) -> int:
    """Main program"""
    parsed = parseargs(arguments[1:])
    if parsed.debug:
        sys.excepthook = idb_excepthook
    if parsed.debug or parsed.verbose:
        logger.setLevel(logging.DEBUG)

    config = parseconfig(parsed.config)

    safaris = discover_safari_profiles()
    all_firefoxes = discover_firefox_profiles()
    all_chromes = discover_chrome_profiles()

    firefoxes = [
        ff
        for ff in all_firefoxes
        if not any([ff.endswith(ig) for ig in config.ignore_firefoxes])
    ]
    chromes = [
        ch
        for ch in all_chromes
        if not any([ch.endswith(ig) for ig in config.ignore_chromes])
    ]

    if parsed.subcmd == "profiles":
        if safaris:
            print("Safari profiles:")
            for profile in safaris:
                print(f"  {profile}")
        else:
            print("No Safari profiles found")
        if all_firefoxes:
            print("Firefox profiles:")
            for profile in all_firefoxes:
                line = f"  {profile}"
                if profile not in firefoxes:
                    line += " (ignored per config)"
                print(line)
        else:
            print("No Firefox profiles found")
        if all_chromes:
            print("Chrome profiles:")
            for profile in all_chromes:
                line = f"  {profile}"
                if profile not in chromes:
                    line += " (ignored per config)"
                print(line)
        else:
            print("No Chrome profiles found")
    elif parsed.subcmd == "urls":
        profiles = []

        if parsed.browsers in ["firefox", "all"]:
            profiles += get_firefox_urls(firefoxes, newerthan=parsed.history_newer_than)
        if parsed.browsers in ["chrome", "all"]:
            profiles += get_chrome_urls(chromes, newerthan=parsed.history_newer_than)
        if parsed.browsers in ["safari", "all"]:
            profiles += get_safari_urls(safaris, newerthan=parsed.history_newer_than)
        if parsed.browsers in ["file", "all"] and parsed.url_file:
            profiles += get_file_urls(parsed.url_file)

        for profile in profiles:
            profile.bookmarks = filter_browser_urls_from_expressions(
                config.urlfilters, profile.bookmarks
            )
            profile.history = filter_browser_urls_from_expressions(
                config.urlfilters, profile.history
            )

        if parsed.format == "html":
            print(profiles_html(profiles, parsed.bookmarks, parsed.history))
        elif parsed.format == "markdown":
            print(profiles_markdown(profiles, parsed.bookmarks, parsed.history))
        elif parsed.format == "urls-dedup":
            print(
                profiles_urls_txt(
                    profiles, parsed.bookmarks, parsed.history, dedup=True
                )
            )
        elif parsed.format == "urls":
            print(
                profiles_urls_txt(
                    profiles, parsed.bookmarks, parsed.history, dedup=False
                )
            )
        elif parsed.format == "domainstats":
            print(profiles_urls_domainstats(profiles, parsed.bookmarks, parsed.history))
        else:
            raise Exception(f"Unknown value for --format: {parsed.format}")
    else:
        raise Exception(f"Unknown subcommand: {parsed.subcmd}")

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
