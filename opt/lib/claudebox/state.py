"""Claudebox state helpers."""

import json
import os
import sys
from typing import Any, cast

from .config import Config
from .errors import die
from .paths import script_repo_root


def _merge_json_defaults(path: str, defaults: dict[str, Any]) -> None:
    """Set missing keys (recursively) in a JSON file, creating it if absent.
    Existing values always win; unparseable files are left alone."""
    data: dict[str, Any]
    try:
        with open(path) as f:
            data = json.load(f)
    except FileNotFoundError:
        data = {}
    except ValueError:
        return

    def merge(dst: dict[str, Any], src: dict[str, Any]) -> bool:
        changed = False
        for key, val in src.items():
            if key not in dst:
                dst[key] = val
                changed = True
            elif isinstance(dst[key], dict) and isinstance(val, dict):
                changed = (
                    merge(cast("dict[str, Any]", dst[key]), cast("dict[str, Any]", val))
                    or changed
                )
        return changed

    if merge(data, defaults):
        with open(path, "w") as f:
            json.dump(data, f, indent=2)
            f.write("\n")


def ensure_statedir(config: Config) -> None:
    """Create the state dir and keep hostpath.txt correct."""
    os.makedirs(config.homedir, exist_ok=True)

    # Seed Claude Code config so fresh sessions trust /src, skip the
    # --dangerously-skip-permissions warning, and use the fullscreen TUI.
    claude_dir = os.path.join(config.homedir, ".claude")
    os.makedirs(claude_dir, exist_ok=True)
    _merge_json_defaults(
        os.path.join(claude_dir, "settings.json"),
        {"skipDangerousModePermissionPrompt": True, "tui": "fullscreen"},
    )
    _merge_json_defaults(
        os.path.join(config.homedir, ".claude.json"),
        {
            "hasCompletedOnboarding": True,
            "projects": {"/src": {"hasTrustDialogAccepted": True}},
        },
    )
    hostpath_file = os.path.join(config.homedir, "hostpath.txt")
    if os.path.exists(hostpath_file):
        with open(hostpath_file) as f:
            recorded = f.read().strip()
        if recorded != config.host_path:
            print(
                f"claudebox: warning: {hostpath_file} contains '{recorded}' but expected '{config.host_path}'; fixing",
                file=sys.stderr,
            )
            with open(hostpath_file, "w") as f:
                f.write(config.host_path + "\n")
    else:
        with open(hostpath_file, "w") as f:
            f.write(config.host_path + "\n")


def shared_pi_agent_dir() -> str:
    """Pi configuration shared by all projects; sessions live elsewhere."""
    path = os.path.expanduser("~/.local/state/claudebox/shared-pi-agent")
    os.makedirs(path, mode=0o700, exist_ok=True)
    # claudebox overlays dhd's managed extension bundle below this directory.
    os.makedirs(os.path.join(path, "extensions", "dhd"), exist_ok=True)

    defaults_path = os.path.join(
        script_repo_root(), "opt", "pi", "settings.defaults.json"
    )
    try:
        with open(defaults_path) as f:
            loaded: Any = json.load(f)
    except (OSError, ValueError) as exc:
        die(f"cannot load Pi settings defaults from {defaults_path}: {exc}")
    if not isinstance(loaded, dict):
        die(f"Pi settings defaults in {defaults_path} must be a JSON object")
    _merge_json_defaults(
        os.path.join(path, "settings.json"), cast("dict[str, Any]", loaded)
    )
    return path


def configure_paseo_state(config: Config) -> None:
    """Persist claudebox's non-interactive Paseo mode settings.

    Preserve unrelated Paseo configuration, but make the two startup choices
    owned by this mode explicit: relay on, voice/dictation off.
    """
    paseo_dir = os.path.join(config.homedir, ".paseo")
    os.makedirs(paseo_dir, exist_ok=True)
    path = os.path.join(paseo_dir, "config.json")
    try:
        with open(path) as f:
            loaded: Any = json.load(f)
    except FileNotFoundError:
        loaded = {}
    except (ValueError, TypeError):
        die(f"cannot configure Paseo because {path} is not valid JSON")

    if not isinstance(loaded, dict):
        die(f"cannot configure Paseo because {path} must contain a JSON object")
    data = cast("dict[str, Any]", loaded)

    def object_field(parent: dict[str, Any], key: str) -> dict[str, Any]:
        value = parent.get(key)
        if value is None:
            child: dict[str, Any] = {}
            parent[key] = child
            return child
        if not isinstance(value, dict):
            die(
                f"cannot configure Paseo because {path} field '{key}' "
                "must be a JSON object"
            )
        return cast("dict[str, Any]", value)

    data.setdefault("$schema", "https://paseo.sh/schemas/paseo.config.v1.json")
    data.setdefault("version", 1)
    daemon = object_field(data, "daemon")
    features = object_field(data, "features")
    relay = object_field(daemon, "relay")
    dictation = object_field(features, "dictation")
    voice_mode = object_field(features, "voiceMode")
    relay["enabled"] = True
    dictation["enabled"] = False
    voice_mode["enabled"] = False
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def cmd_statedir(config: Config) -> None:
    ensure_statedir(config)
    print(
        f"Shortened hash of {config.host_path} is {config.proj_hash}", file=sys.stderr
    )
    print(config.homedir)
