"""Claudebox errors helpers."""

import sys
from typing import NoReturn


def die(msg: str) -> NoReturn:
    print(f"claudebox: {msg}", file=sys.stderr)
    sys.exit(1)
