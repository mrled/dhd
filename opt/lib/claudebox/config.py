"""Claudebox config helpers."""

from dataclasses import dataclass
import os
import shutil

from .errors import die
from .hashing import layer_chain_hash, sha256_short
from .paths import script_repo_root


@dataclass
class Config:
    homedir: str
    # Ordered image layers to build: (containerfile, build_context). Optional
    # prepend layers come first, then the base image, then optional append
    # layers; each layer after the first is built on top of the previous via
    # the CLAUDEBOX_BASE build arg.
    layers: list[tuple[str, str]]
    # Index of the base image within layers (prepends precede it).
    base_index: int
    build_override: str | None
    run_override: str | None
    tag: str
    git_root: str | None
    extra_env: list[str]
    extra_mounts: list[str]
    claudebox_dir: str | None
    netwhitelist: str | None
    hostports: list[int]
    host_path: str
    proj_hash: str


def _parse_lines(path: str) -> list[str]:
    with open(path) as f:
        return [x.strip() for x in f if x.strip() and not x.strip().startswith("#")]


def _parse_hostports(path: str) -> list[int]:
    ports: list[int] = []
    with open(path) as f:
        for raw in f:
            line = raw.partition("#")[0].strip()
            if not line:
                continue
            if not line.isascii() or not line.isdecimal() or len(line) > 5:
                die(f"invalid TCP port in {path}: {line!r}")
            port = int(line)
            if not 1 <= port <= 65535:
                die(f"invalid TCP port in {path}: {line!r}")
            ports.append(port)
    return ports


def _resolve_mounts(lines: list[str], claudebox_parent: str) -> list[str]:
    result: list[str] = []
    for line in lines:
        parts = line.split(":", 2)
        host = parts[0]
        if not os.path.isabs(host) and not host.startswith("~"):
            host = os.path.join(claudebox_parent, host)
        else:
            host = os.path.expanduser(host)
        result.append(":".join([host] + parts[1:]))
    return result


def resolve_config(claudebox_dir: str | None, git_root: str | None) -> Config:
    builtin_dir = os.path.join(script_repo_root(), "opt", "lib", "claudebox", "container")
    builtin_base = os.path.join(builtin_dir, "Containerfile.base")
    user_dir = os.path.expanduser("~/.config/claudebox")
    user_base = os.path.join(user_dir, "Containerfile.base")
    user_prepend = os.path.join(user_dir, "Containerfile.prepend")
    user_append = os.path.join(user_dir, "Containerfile.append")

    if claudebox_dir:
        hash_input = claudebox_dir
        host_path = os.path.dirname(claudebox_dir)
    elif git_root:
        hash_input = os.path.realpath(git_root)
        host_path = hash_input
    else:
        hash_input = os.path.realpath(os.getcwd())
        host_path = hash_input
    proj_hash = sha256_short(hash_input)

    project_base: str | None = None
    project_prepend: str | None = None
    project_prepend_context: str | None = None
    project_append: str | None = None
    build_override: str | None = None
    run_override: str | None = None
    tag_override: str | None = None

    if claudebox_dir:
        p = os.path.join(claudebox_dir, "Containerfile.base")
        project_base = p if os.path.isfile(p) else None
        p = os.path.join(claudebox_dir, "Containerfile.prepend")
        project_prepend = p if os.path.isfile(p) else None
        p = os.path.join(claudebox_dir, "Containerfile.prepend.path")
        if os.path.isfile(p):
            if project_prepend:
                die(
                    f"{claudebox_dir} cannot contain both Containerfile.prepend "
                    "and Containerfile.prepend.path"
                )
            with open(p) as f:
                target = f.read().strip()
            if not target:
                die(f"{p} is empty")
            if not os.path.isabs(target) and not target.startswith("~"):
                target = os.path.join(claudebox_dir, target)
            project_prepend = os.path.realpath(os.path.expanduser(target))
            if not os.path.isfile(project_prepend):
                die(f"prepend Containerfile not found: {project_prepend}")
            project_prepend_context = os.path.dirname(project_prepend)
        p = os.path.join(claudebox_dir, "Containerfile.append")
        project_append = p if os.path.isfile(p) else None
        p = os.path.join(claudebox_dir, "build.sh")
        build_override = p if os.path.isfile(p) else None
        p = os.path.join(claudebox_dir, "run.sh")
        run_override = p if os.path.isfile(p) else None
        p = os.path.join(claudebox_dir, "tag.txt")
        if os.path.isfile(p):
            with open(p) as f:
                tag_override = f.read().strip()

    # Base layer: most specific base wins (project > user > built-in), replacing
    # the base image entirely. Appends always stack on top regardless of which
    # base won, in order of increasing specificity (user then project).
    if project_base:
        # project_base is only set when claudebox_dir is (see above).
        assert claudebox_dir is not None
        base_cf, base_ctx = project_base, claudebox_dir
    elif os.path.isfile(user_base):
        base_cf, base_ctx = user_base, user_dir
    elif os.path.isfile(builtin_base):
        base_cf, base_ctx = builtin_base, builtin_dir
    else:
        die(f"base Containerfile not found at {user_base} or {builtin_base}")

    # Prepend layers build BEFORE the base (user then project), for setup the
    # base build itself depends on -- e.g. trusting a private CA before the base
    # pulls toolchains from the network. The base's FROM takes the last prepend
    # via the CLAUDEBOX_BASE build arg, defaulting to a stock image without one.
    layers: list[tuple[str, str]] = []
    if os.path.isfile(user_prepend):
        layers.append((user_prepend, user_dir))
    if project_prepend:
        # project_prepend is only set when claudebox_dir is (see above). A
        # pointer uses the target Containerfile's directory as its context.
        assert claudebox_dir is not None
        layers.append(
            (project_prepend, project_prepend_context or claudebox_dir)
        )
    base_index = len(layers)
    layers.append((base_cf, base_ctx))
    if os.path.isfile(user_append):
        layers.append((user_append, user_dir))
    if project_append:
        # project_append is only set when claudebox_dir is (see above).
        assert claudebox_dir is not None
        layers.append((project_append, claudebox_dir))

    extra_env: list[str] = []
    extra_mounts: list[str] = []
    netwhitelist: str | None = None
    hostports: list[int] = []
    for directory in (user_dir, claudebox_dir):
        if directory:
            p = os.path.join(directory, "hostports.txt")
            if os.path.isfile(p):
                hostports.extend(_parse_hostports(p))
    hostports = list(dict.fromkeys(hostports))
    if claudebox_dir:
        claudebox_parent = os.path.dirname(claudebox_dir)
        p = os.path.join(claudebox_dir, "env")
        if os.path.isfile(p):
            extra_env = _parse_lines(p)
        p = os.path.join(claudebox_dir, "mounts")
        if os.path.isfile(p):
            extra_mounts = _resolve_mounts(_parse_lines(p), claudebox_parent)
        p = os.path.join(claudebox_dir, "netwhitelist.txt")
        if os.path.isfile(p):
            netwhitelist = p

    homedir = os.path.expanduser(f"~/.local/state/claudebox/{proj_hash}")

    if tag_override:
        tag = tag_override
    elif build_override:
        tag = f"claudebox-{proj_hash}"
    else:
        # Content-addressed over the whole layer chain, so editing any layer
        # triggers a rebuild.
        tag = f"claudebox-{layer_chain_hash(layers)}"

    return Config(
        homedir=homedir,
        layers=layers,
        base_index=base_index,
        build_override=build_override,
        run_override=run_override,
        tag=tag,
        git_root=git_root,
        extra_env=extra_env,
        extra_mounts=extra_mounts,
        claudebox_dir=claudebox_dir,
        netwhitelist=netwhitelist,
        hostports=hostports,
        host_path=host_path,
        proj_hash=proj_hash,
    )


def cmd_config_add(kind: str, git_root: str | None) -> None:
    """Add a starter configuration file to the current project."""
    filenames = {
        "append": "Containerfile.append",
        "prepend": "Containerfile.prepend",
        "env": "env",
        "mounts": "mounts",
        "netwhitelist": "netwhitelist.txt",
        "hostports": "hostports.txt",
    }
    filename = filenames[kind]
    doc_dir = os.path.join(script_repo_root(), "opt", "lib", "claudebox", "doc")
    example = os.path.join(doc_dir, f"{filename}.example")
    project_root = git_root or os.getcwd()
    config_dir = os.path.join(project_root, ".claudebox")
    target = os.path.join(config_dir, filename)

    if os.path.exists(target):
        die(f"refusing to overwrite existing file: {target}")
    if not os.path.isfile(example):
        die(f"built-in example not found: {example}")

    os.makedirs(config_dir, exist_ok=True)
    try:
        with open(example, "rb") as src, open(target, "xb") as dst:
            shutil.copyfileobj(src, dst)
    except FileExistsError:
        die(f"refusing to overwrite existing file: {target}")
    print(f"Created {target}")
