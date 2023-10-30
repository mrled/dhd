# -*- mode:python -*-

import sys
import os
import pip
import platform

# the \001 and \002 stuff came from this useful thread:
# http://stackoverflow.com/questions/9468435/look-how-to-fix-column-calculation-in-python-readline-if-use-color-prompt

# On windows, can set whole screen like this:
# ctypes.windll.Kernel32.GetStdHandle.restype = ctypes.c_ulong
# h = ctypes.windll.Kernel32.GetStdHandle(ctypes.c_ulong(0xfffffff5))
# for color in range(16):
#     ctypes.windll.Kernel32.SetConsoleTextAttribute(h, color)
#     print("hello")

vt = platform.python_version_tuple()

# ignore minor releases: py3.2 instead of py3.2.3
pv = "py" + vt[0] + "." + vt[1]

if os.name == "nt":
    try:
        import colorama
    except ImportError:
        print("colorama is not installed, installing...")
        pip.main(["install", "colorama"])
        import colorama
    # no way to do this on NT without colorama :(
    # colortype = "none"
    colorama.init()
    colortype = "colorama"

    try:
        import pyreadline as readline
    except ImportError:
        print("pyreadline is not installed, installing...")
        pip.main(["install", "pyreadline"])
        import pyreadline as readline

# elif os.name == "posix":
else:
    colortype = "ansi"
    import readline

# set default ps1 and ps2; used by default if I can't colorize
newps1 = pv + " >>> "
newps2 = " " * len(pv) + " ... "

if colortype == "colorama":

    newps1 = (
        colorama.Style.BRIGHT
        + colorama.Fore.GREEN
        + pv
        + " >>> "
        + colorama.Style.RESET_ALL
    )
    newps2 = (
        colorama.Style.BRIGHT
        + colorama.Fore.GREEN
        + "     "
        + " ... "
        + colorama.Style.RESET_ALL
    )

elif colortype == "ansi":
    ansi_norm = "\001\033[0m\002"
    ansi_bold = "\001\033[1m\002"
    ansi_blink = "\001\033[5m\002"
    ansi_rev = "\001\033[7m\002"
    ansi_fg_black = "\001\033[30m\002"
    ansi_fg_red = "\001\033[31m\002"
    ansi_fg_green = "\001\033[32m\002"
    ansi_fg_yellow = "\001\033[33m\002"
    ansi_fg_blue = "\001\033[34m\002"
    ansi_fg_magenta = "\001\033[35m\002"
    ansi_fg_cyan = "\001\033[36m\002"
    ansi_fg_white = "\001\033[37m\002"
    ansi_bg_black = "\001\033[40m\002"
    ansi_bg_red = "\001\033[41m\002"
    ansi_bg_green = "\001\033[42m\002"
    ansi_bg_yellow = "\001\033[43m\002"
    ansi_bg_blue = "\001\033[44m\002"
    ansi_bg_magenta = "\001\033[45m\002"
    ansi_bg_cyan = "\001\033[46m\002"
    ansi_bg_white = "\001\033[47m\002"

    term_with_colors = [
        "xterm",
        "xterm-color",
        "xterm-256color",
        "linux",
        "screen",
        "screen-256color",
        "screen-bce",
    ]
    if os.environ.get("TERM") in term_with_colors:
        newps1 = ansi_bold + ansi_fg_green + pv + " >>> " + ansi_norm
        newps2 = ansi_bold + ansi_fg_green + " " * len(pv) + " ... " + ansi_norm

sys.ps1 = newps1
sys.ps2 = newps2


def prettydir(obj):
    """Print a pretty directory listing for the object."""
    print("    dir(%s):" % obj)
    for x in dir(obj):
        print("        %s" % x)
