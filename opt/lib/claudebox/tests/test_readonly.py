"""Read-only project mount regressions; no container runtime required."""

from contextlib import ExitStack, redirect_stderr, redirect_stdout
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from claudebox import cli, run
from claudebox.config import Config


class ExecCaptured(Exception):
    pass


class ReadonlyTests(unittest.TestCase):
    def setUp(self):
        self.config = Config(
            homedir="/test/home", layers=[], base_index=0,
            build_override=None, run_override=None, tag="claudebox:test",
            git_root="/test/project", extra_env=[], extra_mounts=[],
            claudebox_dir="/test/project/.claudebox", netwhitelist=None,
            hostports=[], host_path="/test/project", proj_hash="test",
        )
        self.stack = ExitStack()
        self.addCleanup(self.stack.close)
        self.effects = {}
        for name in ("build_image", "ensure_statedir", "configure_paseo_state",
                     "check_podman_machine_share", "check_rootless_traversable"):
            self.effects[name] = self.stack.enter_context(patch.object(run, name))
        self.effects["image_exists"] = self.stack.enter_context(
            patch.object(run, "image_exists", return_value=True))
        self.effects["makedirs"] = self.stack.enter_context(patch.object(run.os, "makedirs"))
        self.effects["execvp"] = self.stack.enter_context(
            patch.object(run.os, "execvp", side_effect=ExecCaptured))
        self.effects["execve"] = self.stack.enter_context(
            patch.object(run.os, "execve", side_effect=ExecCaptured))
        self.stack.enter_context(patch.object(run, "shared_pi_agent_dir", return_value="/test/pi"))
        self.stack.enter_context(patch.object(run, "script_repo_root", return_value="/repo"))
        # This fake project represents an existing Git repository.
        isdir = run.os.path.isdir
        self.stack.enter_context(patch.object(
            run.os.path, "isdir",
            side_effect=lambda path: path == "/test/project/.git" or isdir(path),
        ))
        self.stack.enter_context(patch.dict(run.os.environ, {}, clear=True))

    def volumes(self, runtime="docker", selinux=False, **kwargs):
        with patch.object(run, "podman_selinux_enabled", return_value=selinux):
            with self.assertRaises(ExecCaptured):
                run.cmd_run(self.config, runtime, ["bash"], **kwargs)
        argv = self.effects["execvp"].call_args.args[1]
        return [argv[i + 1] for i, arg in enumerate(argv) if arg == "--volume"]

    def test_default_project_and_home_remain_writable(self):
        for runtime, selinux in (("docker", False), ("podman", False), ("podman", True)):
            with self.subTest(runtime=runtime, selinux=selinux):
                suffix = ":z" if selinux else ""
                mounts = self.volumes(runtime, selinux)
                self.assertIn("/test/project:/src" + suffix, mounts)
                self.assertIn("/test/home:/home/claude" + suffix, mounts)
                self.assertIn("/test/project/.git:/src/.git:ro" + (",z" if selinux else ""), mounts)
                self.assertIn("/test/project/.claudebox:/src/.claudebox:ro" + (",z" if selinux else ""), mounts)

    def test_readonly_project_preserves_home_and_selinux(self):
        for runtime, selinux in (("docker", False), ("podman", False), ("podman", True)):
            with self.subTest(runtime=runtime, selinux=selinux):
                mounts = self.volumes(runtime, selinux, readonly=True)
                label = ",z" if selinux else ""
                self.assertIn("/test/project:/src:ro" + label, mounts)
                self.assertIn("/test/project/.git:/src/.git:ro" + label, mounts)
                self.assertIn("/test/project/.claudebox:/src/.claudebox:ro" + label, mounts)
                self.assertIn("/test/home:/home/claude" + (":z" if selinux else ""), mounts)
                self.assertIn("/test/pi:/home/claude/.pi/agent" + (":z" if selinux else ""), mounts)

    def test_extra_project_mounts_normalized_and_forced_readonly(self):
        for runtime, selinux in (("docker", False), ("podman", True)):
            for dst in ("/src", "/src/foo", "//src/foo", "/src/../src/foo", "/src/./foo/", "/other/../src"):
                with self.subTest(runtime=runtime, dst=dst):
                    self.config.extra_mounts = [f"/extra:{dst}:rw,Z,rw,rprivate"]
                    mounts = self.volumes(runtime, selinux, readonly=True)
                    self.assertIn(f"/extra:{dst}:Z,rprivate,ro", mounts)
            self.config.extra_mounts = ["/extra:/src/nested"]
            mounts = self.volumes(runtime, selinux, readonly=True)
            self.assertIn("/extra:/src/nested:ro" + (",z" if selinux else ""), mounts)

    def test_unrelated_extra_mounts_and_default_options_unchanged(self):
        for readonly in (False, True):
            self.config.extra_mounts = ["/extra:/src-other:rw,Z", "/extra:/home/claude/data:rw", "/extra:/src/../other:O"]
            mounts = self.volumes("podman", True, readonly=readonly)
            self.assertIn("/extra:/src-other:rw,Z", mounts)
            self.assertIn("/extra:/home/claude/data:rw,z", mounts)
            self.assertIn("/extra:/src/../other:O,z", mounts)
        self.config.extra_mounts = ["/extra:/src/nested:rw,Z"]
        self.assertIn("/extra:/src/nested:rw,Z", self.volumes())

    def assert_rejected_without_side_effects(self, message):
        stderr = io.StringIO()
        with redirect_stderr(stderr), self.assertRaises(SystemExit) as caught:
            run.cmd_run(self.config, "podman", ["paseo"], readonly=True)
        self.assertEqual(caught.exception.code, 1)
        self.assertIn(message, stderr.getvalue())
        for name, mock in self.effects.items():
            with self.subTest(effect=name):
                mock.assert_not_called()

    def test_override_rejected_before_side_effects(self):
        self.config.run_override = "/test/project/.claudebox/run.sh"
        self.assert_rejected_without_side_effects("run.sh")

    def test_skip_runsh_uses_standard_launcher_with_and_without_readonly(self):
        self.config.run_override = "/test/project/.claudebox/run.sh"
        for readonly in (False, True):
            with self.subTest(readonly=readonly):
                mounts = self.volumes(skip_runsh=True, readonly=readonly)
                self.assertIn("/test/project:/src" + (":ro" if readonly else ""), mounts)
                self.effects["execve"].assert_not_called()
                self.effects["ensure_statedir"].assert_called()
                self.assertEqual(self.config.run_override, "/test/project/.claudebox/run.sh")

    def test_custom_runsh_still_executes_by_default(self):
        self.config.run_override = "/test/project/.claudebox/run.sh"
        with self.assertRaises(ExecCaptured):
            run.cmd_run(self.config, "podman", ["bash"], connection="rootless")
        path, args, env = self.effects["execve"].call_args.args
        self.assertEqual(path, self.config.run_override)
        self.assertEqual(args, [path, "bash"])
        self.assertEqual(env["CONTAINER_CONNECTION"], "rootless")
        self.effects["execvp"].assert_not_called()
        self.effects["ensure_statedir"].assert_not_called()

    def test_writable_overlay_rejected_before_side_effects(self):
        self.config.extra_mounts = ["/extra:/src/../src/nested:O,Z"]
        self.assert_rejected_without_side_effects("overlay")

    def test_cli_flag_and_separator(self):
        cases = [
            (["run", "--readonly", "bash"], ["bash"], True),
            (["run", "bash"], ["bash"], False),
            (["run", "--", "bash", "--readonly"], ["bash", "--readonly"], False),
            (["run", "--", "--readonly"], ["--readonly"], False),
            (["run", "--readonly", "--", "bash", "--readonly"], ["bash", "--readonly"], True),
        ]
        with patch.object(cli, "find_git_root", return_value="/test/project"), \
             patch.object(cli, "find_claudebox_dir", return_value=None), \
             patch.object(cli, "resolve_config", return_value=self.config), \
             patch.object(cli, "find_container_runtime", return_value="docker"), \
             patch.object(cli, "cmd_run") as command:
            for args, expected, readonly in cases:
                with self.subTest(args=args), patch.object(sys, "argv", ["claudebox"] + args):
                    cli.main()
                    command.assert_called_with(self.config, "docker", expected, connection=None, readonly=readonly, skip_runsh=False)
            for args, expected, readonly, skip_runsh in [
                (["run", "--skip-runsh", "bash"], ["bash"], False, True),
                (["run", "--readonly", "--skip-runsh", "bash"], ["bash"], True, True),
                (["run", "--", "bash", "--skip-runsh"], ["bash", "--skip-runsh"], False, False),
            ]:
                with self.subTest(args=args), patch.object(sys, "argv", ["claudebox"] + args):
                    cli.main()
                    command.assert_called_with(self.config, "docker", expected, connection=None, readonly=readonly, skip_runsh=skip_runsh)

    def test_cli_run_help_documents_readonly(self):
        stdout = io.StringIO()
        with patch.object(sys, "argv", ["claudebox", "run", "--help"]), redirect_stdout(stdout):
            with self.assertRaises(SystemExit) as caught:
                cli.main()
        self.assertEqual(caught.exception.code, 0)
        self.assertIn("--readonly", stdout.getvalue())
        self.assertIn("--skip-runsh", stdout.getvalue())
        self.assertIn("read-only", stdout.getvalue())


if __name__ == "__main__":
    unittest.main()
