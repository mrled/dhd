"""Project discovery and mounts outside Git; no live container runtime required."""

from contextlib import ExitStack, redirect_stderr, redirect_stdout
import io
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from claudebox import cli, paths, run
from claudebox.config import resolve_config
from claudebox.hashing import sha256_short


class ExecCaptured(Exception):
    pass


class NoGitTests(unittest.TestCase):
    def setUp(self):
        self.stack = ExitStack()
        self.addCleanup(self.stack.close)
        self.root = Path(self.stack.enter_context(tempfile.TemporaryDirectory())).resolve()
        self.project = self.root / "project"
        self.project.mkdir()
        self.home = self.root / "home"
        self.home.mkdir()
        cwd = os.getcwd()
        os.chdir(self.project)
        self.stack.callback(os.chdir, cwd)
        self.stack.enter_context(patch.dict(os.environ, {"HOME": str(self.home)}, clear=True))

    def capture_volumes(self, config, runtime, readonly, selinux=False):
        with ExitStack() as stack:
            for name in ("build_image", "ensure_statedir", "configure_paseo_state",
                         "check_podman_machine_share", "check_rootless_traversable"):
                stack.enter_context(patch.object(run, name))
            stack.enter_context(patch.object(run, "image_exists", return_value=True))
            stack.enter_context(patch.object(run, "podman_selinux_enabled", return_value=selinux))
            stack.enter_context(patch.object(run, "shared_pi_agent_dir", return_value=str(self.home / "pi")))
            execute = stack.enter_context(patch.object(run.os, "execvp", side_effect=ExecCaptured))
            with self.assertRaises(ExecCaptured):
                run.cmd_run(config, runtime, ["bash"], readonly=readonly)
            argv = execute.call_args.args[1]
        return [argv[i + 1] for i, arg in enumerate(argv) if arg == "--volume"]

    def test_no_git_resolves_cwd_and_never_mounts_or_creates_git(self):
        self.assertIsNone(paths.find_git_root())
        self.assertIsNone(paths.find_claudebox_dir())
        config = resolve_config(None, None)
        self.assertIsNone(config.git_root)
        self.assertEqual(config.host_path, str(self.project))
        self.assertEqual(config.proj_hash, sha256_short(str(self.project)))
        for runtime, selinux in (("docker", False), ("podman", False), ("podman", True)):
            for readonly in (False, True):
                with self.subTest(runtime=runtime, readonly=readonly, selinux=selinux):
                    mounts = self.capture_volumes(config, runtime, readonly, selinux)
                    opts = (["ro"] if readonly else []) + (["z"] if selinux else [])
                    suffix = ":" + ",".join(opts) if opts else ""
                    self.assertIn(f"{self.project}:/src{suffix}", mounts)
                    self.assertFalse(any(mount.split(":")[1] == "/src/.git" for mount in mounts))
                    self.assertFalse(os.path.lexists(self.project / ".git"))

    def test_existing_git_directory_and_worktree_file_have_readonly_overlay(self):
        git = self.project / ".git"
        for kind in ("directory", "file"):
            if kind == "directory":
                git.mkdir()
            else:
                git.write_text("gitdir: /some/repository/.git/worktrees/project\n")
            config = resolve_config(None, str(self.project))
            for runtime, selinux in (("docker", False), ("podman", False), ("podman", True)):
                for readonly in (False, True):
                    with self.subTest(kind=kind, runtime=runtime, readonly=readonly, selinux=selinux):
                        mounts = self.capture_volumes(config, runtime, readonly, selinux)
                        self.assertIn(f"{git}:/src/.git:ro" + (",z" if selinux else ""), mounts)
                        self.assertEqual(git.is_dir(), kind == "directory")
            if kind == "directory":
                git.rmdir()
            else:
                self.assertEqual(git.read_text(), "gitdir: /some/repository/.git/worktrees/project\n")
                git.unlink()

    def test_local_config_detection_and_readonly_overlay_without_git(self):
        local = self.project / ".claudebox"
        local.mkdir()
        (local / "env").write_text("MY_PROJECT_SETTING\n")
        self.assertIsNone(paths.find_git_root())
        self.assertEqual(paths.find_claudebox_dir(), str(local))
        config = resolve_config(paths.find_claudebox_dir(), None)
        self.assertIsNone(config.git_root)
        self.assertEqual(config.extra_env, ["MY_PROJECT_SETTING"])
        self.assertEqual(config.host_path, str(self.project))
        mounts = self.capture_volumes(config, "docker", False)
        self.assertIn(f"{self.project}:/src", mounts)
        self.assertIn(f"{local}:/src/.claudebox:ro", mounts)
        self.assertFalse(any(mount.split(":")[1] == "/src/.git" for mount in mounts))
        self.assertFalse((self.project / ".git").exists())

    def test_cli_config_add_and_statedir_need_no_git_or_runtime(self):
        with patch.object(cli, "find_container_runtime") as runtime:
            for kind, filename in (("append", "Containerfile.append"), ("prepend", "Containerfile.prepend"),
                                   ("env", "env"), ("mounts", "mounts"),
                                   ("netwhitelist", "netwhitelist.txt"), ("hostports", "hostports.txt")):
                with self.subTest(kind=kind), redirect_stdout(io.StringIO()), \
                     patch.object(sys, "argv", ["claudebox", "config", "add", kind]):
                    cli.main()
                    target = self.project / ".claudebox" / filename
                    example = Path(paths.script_repo_root()) / "opt/lib/claudebox/doc" / (filename + ".example")
                    self.assertEqual(target.read_bytes(), example.read_bytes())
            expected = resolve_config(paths.find_claudebox_dir(), None)
            stdout = io.StringIO()
            with redirect_stdout(stdout), redirect_stderr(io.StringIO()), \
                 patch.object(sys, "argv", ["claudebox", "statedir"]):
                cli.main()
            self.assertEqual(stdout.getvalue().strip(), expected.homedir)
            self.assertTrue(Path(expected.homedir).is_dir())
            runtime.assert_not_called()
        self.assertFalse((self.project / ".git").exists())


if __name__ == "__main__":
    unittest.main()
