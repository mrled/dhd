"""Claudebox runtime helpers."""

import json
import os
import shutil
import subprocess
import sys
from typing import Any, cast

from .errors import die


def find_container_runtime() -> str:
    override = os.environ.get("CLAUDEBOX_CONTAINER_RUNTIME")
    if override:
        return override
    for rt in ("docker", "podman"):
        if shutil.which(rt):
            return rt
    die(
        "no container runtime found (tried docker, podman); set CLAUDEBOX_CONTAINER_RUNTIME"
    )


def image_exists(runtime: str, tag: str, connection: str | None = None) -> bool:
    conn = ["--connection", connection] if connection else []
    result = subprocess.run(
        [runtime] + conn + ["image", "inspect", tag],
        capture_output=True,
        check=False,
    )
    return result.returncode == 0


def podman_is_rootless(runtime: str, connection: str | None = None) -> bool:
    """Whether the given Podman connection (or the active default) runs rootless.
    Defaults to False (rootful) if detection fails."""
    conn = ["--connection", connection] if connection else []
    result = subprocess.run(
        [runtime] + conn + ["info", "--format", "{{.Host.Security.Rootless}}"],
        capture_output=True,
        text=True,
        check=False,
    )
    return result.stdout.strip() == "true"


def podman_selinux_enabled(runtime: str, connection: str | None = None) -> bool:
    """Whether a native Linux Podman engine enforces SELinux container labels.

    Bind mounts need a relabel option when SELinux is enabled or their normal
    host labels make them inaccessible in the container. Do not add relabeling
    for Docker, non-Linux Podman clients, or when detection fails.
    """
    if sys.platform != "linux":
        return False
    conn = ["--connection", connection] if connection else []
    result = subprocess.run(
        [runtime] + conn + ["info", "--format", "json"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0 or not result.stdout.strip():
        return False
    try:
        info: dict[str, Any] = json.loads(result.stdout)
    except ValueError:
        return False
    host = info.get("host", info.get("Host", {}))
    if not isinstance(host, dict):
        return False
    security = host.get("security", host.get("Security", {}))
    if not isinstance(security, dict):
        return False
    return security.get("selinuxEnabled", security.get("SELinuxEnabled", False)) is True


def podman_connections(runtime: str) -> list[dict[str, Any]]:
    """All configured Podman system connections (empty on native Linux podman
    with no machine, or if the query fails)."""
    result = subprocess.run(
        [runtime, "system", "connection", "list", "--format", "json"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0 or not result.stdout.strip():
        return []
    try:
        loaded: Any = json.loads(result.stdout)
    except ValueError:
        return []
    if not isinstance(loaded, list) or not all(
        isinstance(item, dict) for item in loaded
    ):
        return []
    return cast("list[dict[str, Any]]", loaded)


def select_rootless_podman(runtime: str) -> str | None:
    """claudebox requires a rootless Podman engine: keep-id (which lines up the
    host user with the container's claude user) is rootless-only, and the rootful
    alternative -- idmapped bind mounts -- isn't available on macOS Podman
    machines (libkrun/virtiofs). If the active connection is already rootless,
    return None (use the default). Otherwise, when Podman machines expose a
    rootless connection (macOS/Windows create both `<machine>` rootless and
    `<machine>-root` rootful), return its name so callers can pass it explicitly
    via --connection -- leaving the user's global default untouched. Die if no
    rootless engine is reachable."""
    if podman_is_rootless(runtime):
        return None
    conns = podman_connections(runtime)
    # Probe non-default connections, trying podman-machine rootless URIs
    # (ssh://core@) before others.
    candidates = [c for c in conns if not c.get("Default")]
    candidates.sort(key=lambda c: 0 if "core@" in c.get("URI", "") else 1)
    for c in candidates:
        name = c.get("Name")
        if isinstance(name, str) and name and podman_is_rootless(runtime, name):
            print(
                f"claudebox: active Podman connection is rootful; using rootless "
                f"connection '{name}' for this run",
                file=sys.stderr,
            )
            return name
    die(
        "Podman is running rootful and no rootless connection was found. "
        "claudebox needs rootless Podman (keep-id).\n"
        "On macOS/Windows, list connections and make the rootless one "
        "(ssh://core@, not ssh://root@) the default:\n"
        "    podman system connection list\n"
        "    podman system connection default <rootless-connection>"
    )


def check_rootless_traversable(path: str) -> None:
    """Under rootless Podman, crun prepares the nested .git bind mount as
    container-root and relies on CAP_DAC_OVERRIDE to traverse the project root --
    but only over a dir whose uid *and* gid are both mapped into the container.
    The macOS VM maps the user's uid and the VM's primary gid (core), not the
    dir's macOS group (usually staff/20), so a 0700 project root is untraversable;
    o+x on the root fixes it while leaving the contents owner-only.

    On Linux rootless the user's own primary gid is mapped, so a dir owned by that
    gid works without o+x -- skip the check there.
    """
    st = os.stat(path)
    if sys.platform != "darwin" and st.st_gid == os.getgid():
        return
    if not st.st_mode & 0o001:
        die(
            f"rootless Podman needs the project root traversable by the container "
            f"user, but '{path}' lacks o+x (other-execute). Fix with:\n"
            f"    chmod o+x {path}\n"
            f"This leaves the directory's contents owner-only."
        )


def _share_root(path: str) -> str:
    """Best-guess directory to share into the Podman machine for `path`: the
    volume root (/Volumes/<name>) for an external/secondary macOS volume,
    otherwise the path itself."""
    parts = path.split(os.sep)
    if len(parts) >= 3 and parts[1] == "Volumes":
        return os.sep.join(parts[:3])
    return path


def _default_podman_machine(runtime: str) -> str | None:
    """Name of the default Podman machine (or the sole machine), or None if it
    can't be determined."""
    result = subprocess.run(
        [runtime, "machine", "list", "--format", "json"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0 or not result.stdout.strip():
        return None
    try:
        machines: list[dict[str, Any]] = json.loads(result.stdout)
    except ValueError:
        return None
    if not machines:
        return None
    for m in machines:
        if m.get("Default"):
            name = m.get("Name")
            return name if isinstance(name, str) else None
    name = machines[0].get("Name")
    return name if isinstance(name, str) else None


def _path_in_podman_machine(runtime: str, machine: str, path: str) -> bool | None:
    """Whether `path` is visible inside the Podman machine's VM. macOS Podman
    mounts shared host directories at the same path inside the guest, so an
    existing path there means the project is reachable. Returns None when it
    can't be told (ssh error/timeout), so callers don't block on uncertainty."""
    try:
        result = subprocess.run(
            [runtime, "machine", "ssh", machine, "test", "-e", path],
            capture_output=True,
            text=True,
            check=False,
            timeout=20,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if result.returncode == 0:
        return True
    if result.returncode == 1:  # `test` ran and reported "does not exist"
        return False
    return None  # ssh-level failure, not a clean test result


def check_podman_machine_share(
    runtime: str, project_root: str, connection: str | None
) -> None:
    """On macOS, Podman runs in a Linux VM that can only bind-mount host
    directories shared into it (by default just your home). A project on an
    unshared volume -- e.g. an external disk under /Volumes -- makes `podman run`
    fail deep in the OCI runtime with an opaque `statfs ... no such file or
    directory`. Detect that here and explain the fix.

    Conservative: paths under home are assumed shared (skipped); otherwise the
    project path is probed inside the VM and we only die on a definitive
    "not present", bailing out on any uncertainty so a working setup is never
    blocked. (The machine's share list isn't in `podman machine inspect`, so the
    guest itself is the source of truth.)"""
    if sys.platform != "darwin":
        return
    real = os.path.realpath(project_root)
    home = os.path.realpath(os.path.expanduser("~"))
    if real == home or real.startswith(home + os.sep):
        return  # home is shared into the machine by default
    # Machine subcommands act on local machine config, so they take a machine
    # name: the rootless connection name is the machine name; the rootful one is
    # `<machine>-root`.
    machine = (
        connection.removesuffix("-root")
        if connection
        else _default_podman_machine(runtime)
    )
    if not machine:
        return
    if _path_in_podman_machine(runtime, machine, project_root) is not False:
        return  # reachable, or couldn't determine -> don't block
    share = _share_root(project_root)
    die(
        "Podman on macOS runs inside a Linux VM that can only bind-mount host\n"
        f"directories shared into it, and this project isn't visible inside the\n"
        f"'{machine}' machine, so the container can't mount it:\n"
        f"    {project_root}\n\n"
        "The share can only be set when the machine is created (there is no\n"
        "`podman machine set --volume`), so recreate it with the volume shared.\n"
        "`--volume` REPLACES the default shares rather than adding to them, so\n"
        "the defaults have to be repeated or your home directory stops working:\n"
        f"    podman machine stop\n"
        f"    podman machine rm {machine}\n"
        f"    podman machine init \\\n"
        f"        --volume /Users:/Users \\\n"
        f"        --volume /private:/private \\\n"
        f"        --volume /var/folders:/var/folders \\\n"
        f"        --volume {share}:{share} \\\n"
        f"        --now\n"
        "This rebuilds the VM (its images/containers are lost; claudebox images\n"
        "are content-addressed and rebuild on next use). Alternatively, move the\n"
        "project under your home directory, which is shared by default."
    )
