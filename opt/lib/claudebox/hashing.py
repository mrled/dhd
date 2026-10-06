"""Claudebox hashing helpers."""

import hashlib
import os

from .paths import script_repo_root


def sha256_short(s: str) -> str:
    return hashlib.sha256(s.encode()).hexdigest()[:16]


def sha256_file_short(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        h.update(f.read())
    return h.hexdigest()[:16]


def layer_chain_hash(layers: list[tuple[str, str]]) -> str:
    """Hash the recipes plus the built-in base's copied runtime inputs."""
    builtin_base = os.path.join(script_repo_root(), "opt", "lib", "claudebox", "container", "Containerfile.base")

    def layer_hash(cf: str, context: str) -> str:
        result = sha256_file_short(cf)
        if os.path.realpath(cf) == os.path.realpath(builtin_base):
            for name in ("entrypoint.sh", "netblocked", "apt-install", "CLAUDE.container.md",
                         "squid.conf", "netwhitelist-default.txt", "paseo-nonroot"):
                result = sha256_short(result + name + sha256_file_short(os.path.join(context, name)))
        return result

    running = layer_hash(*layers[0])
    for cf, context in layers[1:]:
        running = sha256_short(running + layer_hash(cf, context))
    return running
