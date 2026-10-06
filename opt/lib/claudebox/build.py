"""Claudebox build helpers."""

import os
import subprocess
import time

from .config import Config
from .hashing import layer_chain_hash


def build_image(
    config: Config,
    runtime: str,
    extra_args: list[str] | None = None,
    force: bool = False,
    ai_update: bool = False,
    connection: str | None = None,
) -> None:
    args = extra_args or []
    if config.build_override:
        env = os.environ.copy()
        if force:
            env["CLAUDEBOX_FORCE"] = "1"
        if ai_update:
            env["CLAUDEBOX_AIUPDATE"] = "1"
        # Pass the selected rootless connection to the override script via its own
        # env only -- without mutating this process's global environment.
        if connection:
            env["CONTAINER_CONNECTION"] = connection
        subprocess.run([config.build_override] + args, check=True, env=env)
        return
    force_args = ["--no-cache"] if force else []
    # A changing CLAUDEBOX_AI_CACHEBUST value invalidates the base layer's AI
    # install step, reinstalling the agents while reusing the rest of the cache.
    # Only the base layer declares the ARG; downstream layers rebuild
    # automatically because their FROM parent image changes.
    ai_build_arg = (
        ["--build-arg", f"CLAUDEBOX_AI_CACHEBUST={int(time.time())}"]
        if ai_update
        else []
    )

    conn = ["--connection", connection] if connection else []

    def run_build(build_args: list[str], extra: list[str]) -> None:
        subprocess.run(
            [runtime]
            + conn
            + ["build", "--progress=plain"]
            + build_args
            + force_args
            + extra,
            check=True,
        )

    # Build each layer in order (prepends, base, appends); each layer after the
    # first is built on top of the previous via the CLAUDEBOX_BASE build arg
    # (the first layer's FROM uses its own default). Intermediate layers are
    # content-addressed (cached, so cheap when unchanged); extra CLI args apply
    # only to the final image's build.
    layers = config.layers
    last = len(layers) - 1
    prev_tag: str | None = None
    for i, (cf, ctx) in enumerate(layers):
        layer_tag = (
            config.tag
            if i == last
            else f"claudebox-{layer_chain_hash(layers[: i + 1])}"
        )
        build_args = ["--tag", layer_tag, "--file", cf]
        if prev_tag is not None:
            build_args += ["--build-arg", f"CLAUDEBOX_BASE={prev_tag}"]
        if i == config.base_index:
            build_args += ai_build_arg
        build_args.append(ctx)
        run_build(build_args, args if i == last else [])
        prev_tag = layer_tag
