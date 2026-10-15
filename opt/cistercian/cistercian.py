#!/usr/bin/env python3
"""Two-line Cistercian numerals, drawn on a 5 x 8 Braille-dot grid."""

import argparse
from datetime import datetime
import re
import sys
import time


# Braille dot numbers, indexed by row then column within a 2 x 4 cell.
BITS = ((0, 3), (1, 4), (2, 5), (6, 7))


def parse_number(text: str) -> int:
    if not re.fullmatch(r"[0-9]+", text):
        raise argparse.ArgumentTypeError("expected an integer from 0 to 9999")
    normalized = text.lstrip("0") or "0"
    if len(normalized) > 4:
        raise argparse.ArgumentTypeError("expected an integer from 0 to 9999")
    return int(normalized)


def stroke(digit: int, x: int, y: int) -> bool:
    """A units-quadrant stroke; x is distance from the central stem."""
    top = y == 0
    bottom = y == 2
    outer = x == 2
    return (
        (digit in (1, 7, 9) and top)
        or (digit in (2, 8, 9) and bottom)
        or (digit == 3 and x == y)
        or (digit == 4 and x + y == 2)
        or (digit == 5 and outer and y == 1)
        or (digit == 6 and outer and (top or bottom))
        or (digit in (7, 8, 9) and outer)
    )


def glyph(number: int) -> tuple[str, str]:
    """Reflect units into tens, hundreds, and thousands around the stem."""
    if not 0 <= number <= 9999:
        raise ValueError("number must be between 0 and 9999")
    # Three-dot quadrants leave rows 3 and 4 as bare stem extensions.
    dots = {(2, y) for y in range(8)}
    for place in range(4):
        digit = number // (10 ** place) % 10
        for y in range(3):
            for x in range(3):
                if stroke(digit, x, y):
                    dots.add((2 - x if place % 2 else 2 + x,
                              7 - y if place >= 2 else y))
    rows = []
    for row in range(2):
        cells = []
        for column in range(3):
            mask = 0
            for y in range(4):
                for x in range(2):
                    if (column * 2 + x, row * 4 + y) in dots:
                        mask |= 1 << BITS[y][x]
            cells.append(chr(0x2800 + mask))
        rows.append("".join(cells))
    return rows[0], rows[1]


def render(numbers: list[int]) -> str:
    glyphs = [glyph(number) for number in numbers]
    return "\n".join(" ".join(g[row] for g in glyphs) for row in range(2))


def clock_text(now: datetime) -> str:
    # Four glyphs: YYYY, MMDD, HHMM, seconds (local time).
    return render([now.year, now.month * 100 + now.day,
                   now.hour * 100 + now.minute, now.second])


def chart() -> str:
    def labelled_row(numbers: list[int], labels: list[str]) -> str:
        glyphs = [glyph(number) for number in numbers]
        lines = [" ".join(g[row].center(4) for g in glyphs) for row in range(2)]
        lines.append(" ".join(label.center(4) for label in labels))
        return "\n".join(lines)

    digits = list(range(10))
    places = [1, 10, 100, 1000, 1111]
    return (labelled_row(digits, [f"{n:04d}" for n in digits]) + "\n\n" +
            labelled_row(places, [str(n) for n in places]))


def watch() -> None:
    if not sys.stdout.isatty():
        raise SystemExit("watch requires a terminal")
    previous = None
    try:
        sys.stdout.write("\033[?25l")  # Hide cursor while redrawing.
        while True:
            now = datetime.now()
            current = now.replace(microsecond=0)
            if current != previous:
                # Start below the invoking prompt; revisit only our two rows.
                if previous is not None:
                    sys.stdout.write("\033[2A")
                for line in clock_text(now).splitlines():
                    sys.stdout.write("\r\033[2K" + line + "\n")
                sys.stdout.flush()
                previous = current
            time.sleep(0.05)
    except KeyboardInterrupt:
        pass
    finally:
        sys.stdout.write("\033[?25h")
        sys.stdout.flush()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    numbers = commands.add_parser("number", help="render integers from 0 to 9999")
    numbers.add_argument("numbers", nargs="+", type=parse_number)
    commands.add_parser("time", help="show local date/time as YYYY MMDD HHMM seconds")
    commands.add_parser("watch", help="live local date/time below the prompt; Ctrl-C exits")
    commands.add_parser("chart", help="show labelled digits 0–9 and decimal-place examples")
    args = parser.parse_args()
    if args.command == "number":
        print(render(args.numbers))
    elif args.command == "time":
        print(clock_text(datetime.now()))
    elif args.command == "chart":
        print(chart())
    else:
        watch()


if __name__ == "__main__":
    main()
