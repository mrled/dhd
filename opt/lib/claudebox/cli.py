"""Claudebox cli helpers."""

import argparse
import os

from .build import build_image
from .config import cmd_config_add, resolve_config
from .hashing import sha256_short
from .paths import find_claudebox_dir, find_git_root
from .run import cmd_run
from .runtime import find_container_runtime, select_rootless_podman
from .state import cmd_statedir


def cmd_hash(directory: str, literal: bool) -> None:
    if literal:
        directory = os.path.abspath(directory)
    else:
        directory = os.path.realpath(directory)
    print(sha256_short(directory))


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="claudebox",
        description="Containerized Claude Code runner with per-project overrides.",
        epilog=(
            "The image is built from prepend layers, a base, then append layers. Three\n"
            "files control this, each available system-wide (~/.config/claudebox/) and\n"
            "per-project (.claudebox/):\n"
            "  Containerfile.prepend layers built BEFORE the base, for setup the base\n"
            "                        build itself needs -- e.g. trusting a private CA\n"
            "                        before the base pulls toolchains from the network.\n"
            "                        All prepends apply, system-wide first then project;\n"
            "                        each must begin:\n"
            "                            ARG CLAUDEBOX_BASE=debian:stable-slim\n"
            "                            FROM ${CLAUDEBOX_BASE}\n"
            "                        (the default is used for the first prepend; later\n"
            "                        layers receive the previous layer's tag).\n"
            "  Containerfile.prepend.path\n"
            "                        project-relative path to an existing prepend\n"
            "                        Containerfile; its directory is the build context.\n"
            "  Containerfile.base    replaces the base image entirely. Most specific wins:\n"
            "                        project > system-wide > built-in. A custom base must\n"
            "                        use the same ARG/FROM opening as prepends for prepend\n"
            "                        layers to take effect under it.\n"
            "  Containerfile.append  extra layers built on top of the base. All appends\n"
            "                        apply, system-wide first then project; each must begin:\n"
            "                            ARG CLAUDEBOX_BASE\n"
            "                            FROM ${CLAUDEBOX_BASE}\n"
            "For all three, the build context is the file's own directory.\n"
            "\n"
            "Pi's ~/.pi/agent configuration is shared through\n"
            "~/.local/state/claudebox/shared-pi-agent for every standard run; Pi\n"
            "sessions remain in each project's separate home directory. Claude and\n"
            "Codex configuration and history both remain project-specific.\n"
            "Dhd-managed Pi extensions load from opt/pi/extensions/index.ts.\n"
            "\n"
            "Other project configuration via the .claudebox/ directory (at cwd or git root):\n"
            "  build.sh            replaces the standard 'docker build' invocation\n"
            "  run.sh              replaces the standard 'docker run' invocation\n"
            "  tag.txt             override the image tag name\n"
            "  env                 env var names to pass through (one per line, # comments ok)\n"
            "                      Host PI_* env vars are passed through automatically\n"
            "                      (except claudebox-managed PI_CODING_AGENT_SESSION_DIR).\n"
            "  mounts              extra volume mounts: host:container[:opts], one per line;\n"
            "                      relative host paths resolve from the .claudebox parent dir\n"
            "  hostports.txt       Docker/Podman: allow HTTP and CONNECT via Squid to the host\n"
            "                      gateway on listed TCP ports (one per line, # comments). Entries\n"
            "                      combine from ~/.config/claudebox/hostports.txt and\n"
            "                      .claudebox/hostports.txt; does not restrict public domains.\n"
            "  netwhitelist.txt    restrict container egress to these domains plus built-in\n"
            "                      defaults (AI endpoints, read-only package hosts); one per\n"
            "                      line, # comments ok; 'host.com' matches exactly while\n"
            "                      '.host.com' includes subdomains\n"
            "\n"
            "Set CLAUDEBOX_NETRESTRICT=1 to restrict egress to the built-in endpoints even\n"
            "without a netwhitelist.txt. Inside the container, CLAUDEBOX_NETWORK reports the\n"
            "active mode ('restricted' or 'unrestricted'). In both modes, direct egress to\n"
            "private/link-local address space -- the container host (host.docker.internal /\n"
            "host.containers.internal), its LAN, CGNAT/tailnets -- is blocked, except DNS to\n"
            "the configured resolver. hostports.txt allows access to host.docker.internal\n"
            "or host.containers.internal on only those TCP ports via Squid in either mode.\n"
            "Hostports alone keeps public egress unrestricted and exports proxy settings.\n"
            "Host services must listen on an address reachable by the runtime; native\n"
            "rootless Podman's default network may not reach loopback-only services.\n"
            "Other private destinations stay blocked. This guard needs the container\n"
            "started as root with NET_ADMIN (claudebox always does); containers run\n"
            "by hand without that warn and report CLAUDEBOX_NETWORK=unguarded."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    subparsers = parser.add_subparsers(dest="subcommand")
    build_parser = subparsers.add_parser("build", help="Build the container image")
    build_parser.add_argument(
        "--full",
        action="store_true",
        help="Full rebuild from scratch (passes --no-cache to the container runtime)",
    )
    build_parser.add_argument(
        "--aiupdate",
        action="store_true",
        help="Reinstall just the AI agents (claude-code, codex) to pick up updates, reusing the rest of the cache",
    )
    run_parser = subparsers.add_parser(
        "run", help="Run a command in the container (default)"
    )
    run_parser.add_argument(
        "--readonly",
        action="store_true",
        help="Mount the project read-only (home remains writable; use --skip-runsh for custom run.sh)",
    )
    run_parser.add_argument(
        "--skip-runsh",
        action="store_true",
        help="Ignore custom run.sh and use the standard container launcher",
    )
    run_parser.add_argument(
        "CMD",
        nargs="*",
        metavar="CMD",
        help="Command to run: 'claude' (default), 'codex', 'paseo', or arbitrary command+args",
    )
    subparsers.add_parser(
        "statedir", help="Show the state directory for the current project"
    )
    hash_parser = subparsers.add_parser(
        "hash", help="Show the project hash for a directory"
    )
    hash_parser.add_argument(
        "dir", nargs="?", default=None, help="Directory to hash (default: cwd)"
    )
    hash_parser.add_argument(
        "--literal",
        action="store_true",
        help="Use the literal path string without resolving realpath",
    )
    config_parser = subparsers.add_parser(
        "config", help="Manage configuration for the current project"
    )
    config_subparsers = config_parser.add_subparsers(
        dest="config_subcommand", required=True
    )
    config_add_parser = config_subparsers.add_parser(
        "add", help="Add a starter configuration file"
    )
    config_add_parser.add_argument(
        "kind",
        choices=("append", "prepend", "env", "mounts", "netwhitelist", "hostports"),
        help="Configuration example to add",
    )

    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print tag and home volume directory",
    )

    ns, remaining = parser.parse_known_args()
    subcommand: str = ns.subcommand or "run"

    if remaining and remaining[0] == "--":
        remaining = remaining[1:]

    if subcommand == "hash":
        directory = ns.dir if ns.dir is not None else os.getcwd()
        cmd_hash(directory, ns.literal)
        return

    # ns.CMD (run subparser positionals) + remaining (unrecognised flags) = full cmd.
    cmd_args = getattr(ns, "CMD", []) + remaining

    git_root = find_git_root()

    if subcommand == "config":
        if ns.config_subcommand == "add":
            cmd_config_add(ns.kind, git_root)
        return

    claudebox_dir = find_claudebox_dir()
    config = resolve_config(claudebox_dir, git_root)

    if subcommand == "statedir":
        cmd_statedir(config)
        return

    runtime = find_container_runtime()

    # Ensure a rootless Podman engine before any build/run/image query, selecting
    # a rootless connection if the default is rootful (or exiting if none exists).
    # The selected connection (if any) is threaded explicitly to each podman
    # invocation via --connection rather than through the global environment.
    connection: str | None = None
    if os.path.basename(runtime) == "podman":
        connection = select_rootless_podman(runtime)

    if ns.verbose:
        print(f"tag: {config.tag}")
        print(f"home: {config.homedir}")

    if subcommand == "build":
        build_image(
            config,
            runtime,
            cmd_args,
            force=getattr(ns, "full", False),
            ai_update=getattr(ns, "aiupdate", False),
            connection=connection,
        )
    else:
        cmd_run(
            config, runtime, cmd_args, connection=connection,
            readonly=getattr(ns, "readonly", False),
            skip_runsh=getattr(ns, "skip_runsh", False),
        )
