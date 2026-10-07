# Claudebox Python package

Add the repository's `opt/bin` to `PATH` and run `claudebox` (no installation
needed). `claudebox2` is a compatibility symlink. The launcher resolves its real
path, so it also works through an external symlink or from another directory.
For direct package use, set `PYTHONPATH` to the repository's `opt/lib` and run
`python3 -m claudebox`.

Use `claudebox run --readonly [CMD ...]` to mount the project at `/src`
read-only, including extra mounts targeting `/src` or its children. Writable
`O` overlay mounts there are rejected. The persistent home and unrelated mounts
remain writable; this is not a read-only container. To bypass a custom `run.sh`,
use `claudebox run --skip-runsh`, or combine it with `--readonly`. Other project
configuration (including `build.sh`) still applies. Without `--skip-runsh`, custom
`run.sh` overrides are unsupported with `--readonly`. Use `--` before child
arguments to forward flags literally, e.g. `claudebox run -- tool --readonly`.

Modules:
- `errors`: fatal diagnostics
- `paths`: repository and project discovery
- `hashing`: project and image-layer hashes
- `config`: configuration resolution and starter files
- `runtime`: runtime discovery, Podman queries and mount preflights
- `build`: image builds
- `state`: persistent state, Pi defaults and Paseo settings
- `run`: container commands, mounts, environment and execution
- `cli`: argument parsing and dispatch

Imports flow from CLI/run/build/state/config to the lower-level helpers; helper
modules do not import CLI or run. Within `opt/lib/claudebox`:
- Python modules at the root run on the host.
- `container/` contains the Containerfile and runtime inputs; it is the build context.
- `doc/` contains reference examples, including the Containerfile examples.
- `tests/` contains the regression suite.

Pi assets remain in `opt/pi`; user configuration and persistent state paths are unchanged.
Persistent home settings are seeded by `state.py`, not a checked-in home template.

Tests: `python3 -m unittest discover -s opt/lib/claudebox/tests -v`.
