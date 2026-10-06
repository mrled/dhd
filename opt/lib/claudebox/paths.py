"""Claudebox paths helpers."""

import os
import subprocess


def script_repo_root() -> str:
    """Resolve opt/lib/claudebox/paths.py back to the repository root."""
    return os.path.dirname(
        os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
    )


def find_git_root() -> str | None:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None


def find_claudebox_dir() -> str | None:
    """Return first .claudebox/ found in PWD then git root, or None."""
    candidates = [os.getcwd()]
    git_root = find_git_root()
    if git_root and os.path.realpath(git_root) != os.path.realpath(os.getcwd()):
        candidates.append(git_root)
    for base in candidates:
        d = os.path.join(base, ".claudebox")
        if os.path.isdir(d):
            return os.path.realpath(d)
    return None
