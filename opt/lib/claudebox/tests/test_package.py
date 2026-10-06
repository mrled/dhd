"""Package and executable smoke tests; no container runtime required."""

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


REPO = Path(__file__).resolve().parents[4]
LIB = REPO / "opt/lib"
BIN = REPO / "opt/bin"


class PackageSmokeTests(unittest.TestCase):
    def test_launchers_help_from_unrelated_cwd(self):
        with tempfile.TemporaryDirectory() as directory:
            external = Path(directory) / "external-claudebox"
            external.symlink_to(BIN / "claudebox")
            env = os.environ.copy()
            env.pop("PYTHONPATH", None)
            for launcher in (BIN / "claudebox", BIN / "claudebox2", external):
                with self.subTest(launcher=launcher):
                    result = subprocess.run(
                        [str(launcher), "--help"], cwd=directory, env=env,
                        capture_output=True, text=True,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("usage: claudebox ", result.stdout)
                    self.assertNotIn("claudebox2", result.stdout)

    def test_package_module_and_repo_assets(self):
        with tempfile.TemporaryDirectory() as directory:
            env = dict(os.environ, PYTHONPATH=str(LIB), HOME=directory)
            result = subprocess.run(
                [sys.executable, "-m", "claudebox", "--help"], cwd=directory,
                env=env, capture_output=True, text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            code = """
from pathlib import Path
from claudebox.paths import script_repo_root
from claudebox.config import resolve_config
from claudebox.state import shared_pi_agent_dir
root = Path(script_repo_root())
assert root == Path(__import__('sys').argv[1])
config = resolve_config(None, None)
assert config.layers == [(str(root / 'opt/lib/claudebox/container/Containerfile.base'), str(root / 'opt/lib/claudebox/container'))]
assert (root / 'opt/pi/extensions').is_dir()
assert (Path(shared_pi_agent_dir()) / 'settings.json').is_file()
"""
            result = subprocess.run(
                [sys.executable, "-c", code, str(REPO)], cwd=directory,
                env=env, capture_output=True, text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            result = subprocess.run(
                [str(BIN / "claudebox"), "config", "add", "hostports"],
                cwd=directory, env=env, capture_output=True, text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(
                (Path(directory) / '.claudebox/hostports.txt').read_bytes(),
                (REPO / 'opt/lib/claudebox/doc/hostports.txt.example').read_bytes(),
            )


if __name__ == "__main__":
    unittest.main()
