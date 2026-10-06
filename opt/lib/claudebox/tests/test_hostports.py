"""Hostport command construction regressions; no container runtime required.

Run with: python3 -m unittest discover -s opt/lib/claudebox/tests -v
"""

from pathlib import Path
import sys
import tempfile
import unittest
from contextlib import ExitStack
from unittest.mock import patch


sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from claudebox.config import Config
from claudebox import hashing
from claudebox import runtime as container_runtime
from claudebox import run as claudebox


class ExecCaptured(Exception):
    """Stand in for execvp's non-returning handoff to the runtime."""


class HostportsTests(unittest.TestCase):
    def run_container(self, runtime, hostports, connection=None, restricted=False, netwhitelist=None):
        config = Config(
            homedir="/test/home",
            layers=[],
            base_index=0,
            build_override=None,
            run_override=None,
            tag="claudebox:test",
            git_root="/test/project",
            extra_env=[],
            extra_mounts=[],
            claudebox_dir=None,
            netwhitelist=netwhitelist,
            hostports=hostports,
            host_path="/test/project",
            proj_hash="test-project",
        )
        with ExitStack() as stack:
            stack.enter_context(patch.dict(claudebox.os.environ, {"CLAUDEBOX_NETRESTRICT": "1"} if restricted else {}, clear=True))
            stack.enter_context(patch.object(claudebox, "image_exists", return_value=True))
            build = stack.enter_context(patch.object(claudebox, "build_image"))
            stack.enter_context(patch.object(claudebox, "ensure_statedir"))
            stack.enter_context(patch.object(claudebox.os, "makedirs"))
            stack.enter_context(patch.object(claudebox, "check_podman_machine_share"))
            stack.enter_context(patch.object(claudebox, "check_rootless_traversable"))
            stack.enter_context(
                patch.object(claudebox, "podman_selinux_enabled", return_value=False)
            )
            stack.enter_context(
                patch.object(claudebox, "shared_pi_agent_dir", return_value="/test/pi/agent")
            )
            stack.enter_context(
                patch.object(
                    container_runtime.subprocess,
                    "run",
                    side_effect=AssertionError("unexpected subprocess invocation"),
                )
            )
            execute = stack.enter_context(
                patch.object(claudebox.os, "execvp", side_effect=ExecCaptured)
            )
            with self.assertRaises(ExecCaptured):
                claudebox.cmd_run(config, runtime, ["bash"], connection=connection)
            execute.assert_called_once()
            build.assert_not_called()
            executable, argv = execute.call_args.args
            self.assertEqual(executable, runtime)
            self.assertEqual(argv[-2:], [config.tag, "bash"])
            return argv

    def assert_hostports(self, argv):
        env = [argv[i + 1] for i, arg in enumerate(argv) if arg == "--env"]
        self.assertEqual(env.count("CLAUDEBOX_HOST_PORTS=8080,5432"), 1)
        self.assertNotIn("CLAUDEBOX_NETRESTRICT=1", env)

    def test_builtin_base_runtime_inputs_invalidate_entire_chain(self):
        with tempfile.TemporaryDirectory() as directory:
            base_dir = Path(directory) / "opt/lib/claudebox/container"
            base_dir.mkdir(parents=True)
            base = base_dir / "Containerfile.base"
            base.write_text("FROM fixture\n")
            inputs = ("entrypoint.sh", "netblocked", "apt-install", "CLAUDE.container.md",
                      "squid.conf", "netwhitelist-default.txt", "paseo-nonroot")
            for name in inputs:
                (base_dir / name).write_text("original\n")
            append = base_dir / "Containerfile.append"
            append.write_text("FROM fixture-append\n")
            layers = [(str(base), str(base_dir)), (str(append), str(base_dir))]
            with patch.object(hashing, "script_repo_root", return_value=directory):
                original_base = hashing.layer_chain_hash(layers[:1])
                original_chain = hashing.layer_chain_hash(layers)
                for name in inputs:
                    with self.subTest(name=name):
                        (base_dir / name).write_text("changed\n")
                        self.assertNotEqual(hashing.layer_chain_hash(layers[:1]), original_base)
                        self.assertNotEqual(hashing.layer_chain_hash(layers), original_chain)
                        (base_dir / name).write_text("original\n")
                (base_dir / "hostports.txt.example").write_text("unrelated\n")
                self.assertEqual(hashing.layer_chain_hash(layers), original_chain)

    def test_explicit_restriction_with_hostports(self):
        argv = self.run_container("docker", [8080, 5432], restricted=True)
        self.assertEqual(argv.count("CLAUDEBOX_NETRESTRICT=1"), 1)

    def test_whitelist_with_and_without_hostports_enables_restriction(self):
        for ports in ([], [8080, 5432]):
            with self.subTest(ports=ports):
                argv = self.run_container("docker", ports, netwhitelist="/test/whitelist.txt")
                self.assertEqual(argv.count("CLAUDEBOX_NETRESTRICT=1"), 1)
                self.assertIn("/test/whitelist.txt:/run/claudebox/netwhitelist.txt:ro", argv)

    def test_docker_hostports_add_host_gateway_without_restriction(self):
        argv = self.run_container("docker", [8080, 5432])
        self.assertEqual(argv[:2], ["docker", "run"])
        self.assertEqual(
            argv.count("--add-host=host.docker.internal:host-gateway"), 1
        )
        self.assert_hostports(argv)

    def test_podman_hostports_use_builtin_aliases(self):
        argv = self.run_container("podman", [8080, 5432])
        self.assertEqual(argv[:2], ["podman", "run"])
        self.assertFalse(any(arg.startswith("--add-host") for arg in argv))
        self.assert_hostports(argv)

    def test_podman_hostports_preserve_connection(self):
        argv = self.run_container(
            "/usr/bin/podman", [8080, 5432], connection="rootless-test"
        )
        self.assertEqual(
            argv[:4], ["/usr/bin/podman", "--connection", "rootless-test", "run"]
        )
        self.assertFalse(any(arg.startswith("--add-host") for arg in argv))
        self.assert_hostports(argv)

    def test_no_hostports_adds_no_override_or_restriction(self):
        for runtime in ("docker", "podman"):
            with self.subTest(runtime=runtime):
                argv = self.run_container(runtime, [])
                self.assertFalse(any(arg.startswith("--add-host") for arg in argv))
                self.assertFalse(
                    any(arg.startswith("CLAUDEBOX_HOST_PORTS=") for arg in argv)
                )
                self.assertFalse(
                    any(arg.startswith("CLAUDEBOX_NETRESTRICT=") for arg in argv)
                )


if __name__ == "__main__":
    unittest.main()
