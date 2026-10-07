"""Claudebox run helpers."""

import os
import posixpath
from typing import NoReturn

from .build import build_image
from .config import Config
from .errors import die
from .paths import script_repo_root
from .runtime import (
    check_podman_machine_share,
    check_rootless_traversable,
    image_exists,
    podman_selinux_enabled,
)
from .state import configure_paseo_state, ensure_statedir, shared_pi_agent_dir


def resolve_container_cmd(cmd_args: list[str]) -> list[str]:
    """Expand shorthand CMD tokens into their full container command."""
    if not cmd_args or cmd_args[0] == "claude":
        return ["claude", "--dangerously-skip-permissions"] + cmd_args[1:]
    if cmd_args[0] == "codex":
        return ["codex", "--dangerously-bypass-approvals-and-sandbox"] + cmd_args[1:]
    if cmd_args[0] == "paseo":
        # The wrapper is defense in depth on top of the entrypoint's setpriv:
        # Paseo and every agent it launches must inherit the unprivileged user.
        return ["claudebox-paseo"] + cmd_args[1:]
    return cmd_args


def _container_name(path: str) -> str:
    """Return a Docker-safe container name from the basename of path plus a 4-char random suffix."""
    import re

    name = os.path.basename(path.rstrip("/")) or "claudebox"
    name = re.sub(r"[^a-zA-Z0-9_.-]", "_", name)
    if not name[0].isalnum():
        name = "c" + name
    suffix = os.urandom(2).hex()
    return f"{name}-{suffix}"


def _paseo_hostname(project_root: str) -> str:
    """Return an RFC-1123-style Paseo display hostname derived on the host.

    Paseo reports os.hostname() as its display-name suggestion. Container
    runtimes require a hostname-safe spelling, so combine the invoking machine
    and project directory as `HOST-DIRECTORY-claudebox`.
    """
    import re
    import socket

    def hostname_part(value: str, fallback: str) -> str:
        cleaned = re.sub(r"[^a-zA-Z0-9.-]+", "-", value).strip(".-")
        return cleaned or fallback

    host = hostname_part(socket.gethostname(), "host")
    directory = hostname_part(os.path.basename(project_root.rstrip(os.sep)), "project")
    suffix = "-claudebox"
    value = f"{host}-{directory}{suffix}"
    if len(value) <= 63:
        return value

    # Preserve recognizable pieces of both fields and the fixed suffix.
    available = 63 - len(suffix) - 1
    host_budget = available // 2
    directory_budget = available - host_budget
    host = host[:host_budget].rstrip(".-") or "host"
    directory = directory[:directory_budget].rstrip(".-") or "project"
    return f"{host}-{directory}{suffix}"


def _volume_opt(src: str, dst: str, opts: list[str]) -> str:
    """Build a `src:dst[:opt1,opt2,...]` volume spec, dropping empty options."""
    spec = f"{src}:{dst}"
    present = [o for o in opts if o]
    if present:
        spec += ":" + ",".join(present)
    return spec


def _bind_mount_opts(opts: list[str], selinux_relabel: bool) -> list[str]:
    """Add a shared SELinux label unless the mount already specifies one."""
    if selinux_relabel and not any(opt in ("z", "Z") for opt in opts):
        return opts + ["z"]
    return opts


def _extra_mount_parts(mount: str, readonly: bool) -> tuple[str, str, list[str]]:
    """Enforce read-only options on extra mounts within the project tree."""
    src, _, rest = mount.partition(":")
    dst, _, existing = rest.partition(":")
    opts = existing.split(",") if existing else []
    # Container mount paths treat repeated leading slashes as the root;
    # posixpath otherwise preserves exactly two leading slashes.
    normalized = posixpath.normpath("/" + dst.lstrip("/")) if dst.startswith("/") else dst
    if readonly and (normalized == "/src" or normalized.startswith("/src/")):
        if "O" in opts:
            die(f"--readonly cannot use writable overlay option O for project mount: {mount}")
        opts = [opt for opt in opts if opt not in ("rw", "ro")] + ["ro"]
    return src, dst, opts


def _host_env_passthrough(prefix: str, exclude: set[str] | None = None) -> list[str]:
    """Host environment variable names with the given prefix to pass through."""
    blocked = exclude or set()
    return sorted(
        name for name in os.environ if name.startswith(prefix) and name not in blocked
    )


def cmd_run(
    config: Config,
    runtime: str,
    cmd_args: list[str],
    connection: str | None = None,
    readonly: bool = False,
    skip_runsh: bool = False,
) -> NoReturn:
    if readonly and config.run_override and not skip_runsh:
        die("--readonly is unsupported with a custom run.sh: use --skip-runsh to enforce read-only project mounts")
    # Validate mounts before any exec, build, or persistent state changes.
    extra_mounts = [_extra_mount_parts(mount, readonly) for mount in config.extra_mounts]

    paseo_mode = bool(cmd_args) and cmd_args[0] == "paseo"
    paseo_start_mode = cmd_args == ["paseo"]
    if config.run_override and not skip_runsh:
        # Pass the selected rootless connection to the override script via its own
        # env only -- without mutating this process's global environment.
        env = os.environ.copy()
        if connection:
            env["CONTAINER_CONNECTION"] = connection
        os.execve(config.run_override, [config.run_override] + cmd_args, env)

    project_root = config.git_root or os.getcwd()

    # Preflight the project mount before any (possibly lengthy) image build, so an
    # unshared-volume setup fails fast with guidance instead of deep in podman run.
    if os.path.basename(runtime) == "podman":
        check_podman_machine_share(runtime, project_root, connection)
        check_rootless_traversable(project_root)

    if not image_exists(runtime, config.tag, connection):
        build_image(config, runtime, connection=connection)

    ensure_statedir(config)
    # Pi supports a separate session directory, so its complete global agent
    # directory can be shared without merging project histories. Pre-create the
    # parent because the nested bind mount sits inside the project home mount.
    os.makedirs(os.path.join(config.homedir, ".pi"), exist_ok=True)
    if paseo_start_mode:
        configure_paseo_state(config)

    # Pre-create parent dirs for mounts targeting /home/claude/... so Docker can
    # set up the mountpoint inside the home volume (which doesn't exist yet at bind time).
    for mount in config.extra_mounts:
        parts = mount.split(":", 2)
        container_path = parts[1] if len(parts) >= 2 else ""
        prefix = "/home/claude/"
        if container_path.startswith(prefix):
            rel = container_path[len(prefix) :]
            parent = os.path.dirname(rel)
            if parent:
                os.makedirs(os.path.join(config.homedir, parent), exist_ok=True)

    # One relay daemon may own a project's persistent Paseo identity at a time.
    # A stable name lets the container runtime reject a concurrent second start
    # instead of allowing two PID namespaces to race over paseo.pid and config.
    container_name = (
        f"claudebox-paseo-{config.proj_hash}"
        if paseo_start_mode
        else _container_name(project_root)
    )

    container_cmd = resolve_container_cmd(cmd_args)
    conn = ["--connection", connection] if connection else []
    cmd = [
        runtime,
        *conn,
        "run",
        "--rm",
        "--interactive",
        "--tty",
        "--init",
        "--name",
        container_name,
    ]
    if paseo_start_mode:
        # Paseo uses os.hostname() as the app's suggested host label.
        cmd += ["--hostname", _paseo_hostname(project_root)]
    # Docker Desktop translates bind-mount ownership to the container UID, so the
    # default `claude` (UID 1000) user works. Podman reports real ownership and
    # enforces permissions, so the handling depends on the engine mode, keeping a
    # non-root process in both cases:
    #   - rootless: the host user maps to container UID 0 by default, so a non-root
    #     UID can't own the mounts. keep-id remaps the host user to UID/GID 1000
    #     (the claude user) so ownership lines up.
    #   - rootful: unsupported -- select_rootless_podman() (called in main) has
    #     already switched to a rootless connection or exited, so podman is always
    #     rootless here. keep-id remaps the host user to UID/GID 1000 (claude).
    idmap_opt = ""
    selinux_relabel = False
    if os.path.basename(runtime) == "podman":
        cmd += ["--userns=keep-id:uid=1000,gid=1000"]
        # `z` gives bind-mounted content a shared container label. Shared is
        # intentional: claudebox can run concurrent containers for one project.
        selinux_relabel = podman_selinux_enabled(runtime, connection)
    cmd += [
        "--volume",
        _volume_opt(
            project_root,
            "/src",
            _bind_mount_opts(["ro" if readonly else "", idmap_opt], selinux_relabel),
        ),
        "--volume",
        _volume_opt(
            f"{project_root}/.git",
            "/src/.git",
            _bind_mount_opts(["ro", idmap_opt], selinux_relabel),
        ),
        "--volume",
        _volume_opt(
            config.homedir,
            "/home/claude",
            _bind_mount_opts([idmap_opt], selinux_relabel),
        ),
        "--volume",
        _volume_opt(
            shared_pi_agent_dir(),
            "/home/claude/.pi/agent",
            _bind_mount_opts([idmap_opt], selinux_relabel),
        ),
        "--volume",
        _volume_opt(
            os.path.join(script_repo_root(), "opt", "pi", "extensions"),
            "/home/claude/.pi/agent/extensions/dhd",
            _bind_mount_opts(["ro", idmap_opt], selinux_relabel),
        ),
    ]
    # .claudebox configures claudebox itself -- netwhitelist.txt widens the
    # next session's egress, and run.sh/build.sh/mounts execute or take effect
    # on the host -- so overlay it read-only on the rw /src mount to keep the
    # containerized agent from tampering with it.
    if config.claudebox_dir:
        rel = os.path.relpath(config.claudebox_dir, project_root)
        if not rel.startswith(".."):
            cmd += [
                "--volume",
                _volume_opt(
                    config.claudebox_dir,
                    f"/src/{rel}",
                    _bind_mount_opts(["ro", idmap_opt], selinux_relabel),
                ),
            ]
    cmd += [
        "--env",
        "ANTHROPIC_API_KEY",
        "--env",
        "ANTHROPIC_AUTH_TOKEN",
        "--env",
        "ANTHROPIC_BASE_URL",
        "--env",
        "PI_CODING_AGENT_SESSION_DIR=/home/claude/.pi-sessions",
        "--env",
        "TZ=America/Chicago",
    ]
    for var in _host_env_passthrough(
        "PI_", exclude={"PI_CODING_AGENT_SESSION_DIR"}
    ):
        cmd += ["--env", var]
    if paseo_mode:
        # The no-argument wrapper uses a Unix socket and the outbound relay, so
        # Paseo exposes no host or container TCP port. Its identity and pairing
        # state live under the already-persistent /home/claude bind mount.
        cmd += [
            "--env",
            "PASEO_HOME=/home/claude/.paseo",
        ]
    # The entrypoint always starts as root with NET_ADMIN so it can program nftables and start squid.
    cmd += ["--user", "root", "--cap-add", "NET_ADMIN"]
    if config.hostports:
        # Podman supplies both internal host aliases itself, including on
        # Podman Machine. Do not override its backend-specific host address.
        if os.path.basename(runtime) == "docker":
            cmd += ["--add-host=host.docker.internal:host-gateway"]
        cmd += [
            "--env",
            "CLAUDEBOX_HOST_PORTS=" + ",".join(map(str, config.hostports)),
        ]
    if os.environ.get("CLAUDEBOX_NETRESTRICT") or config.netwhitelist:
        cmd += [
            "--env",
            "CLAUDEBOX_NETRESTRICT=1",
        ]
        if config.netwhitelist:
            cmd += [
                "--volume",
                _volume_opt(
                    config.netwhitelist,
                    "/run/claudebox/netwhitelist.txt",
                    _bind_mount_opts(["ro"], selinux_relabel),
                ),
            ]
    for var in config.extra_env:
        cmd += ["--env", var]
    for src, dst, opts in extra_mounts:
        cmd += [
            "--volume",
            _volume_opt(
                src,
                dst,
                _bind_mount_opts(opts + [idmap_opt], selinux_relabel),
            ),
        ]
    cmd += [config.tag] + container_cmd
    os.execvp(cmd[0], cmd)
