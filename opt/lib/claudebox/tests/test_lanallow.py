"""LAN parser and runtime handoff regressions (no container runtime needed)."""
from contextlib import redirect_stderr
import io
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from claudebox.config import resolve_config
import test_hostports


class LanallowConfigTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.user = self.root / '.config/claudebox'
        self.project = self.root / 'project/.claudebox'
        self.user.mkdir(parents=True)
        self.project.mkdir(parents=True)
        patcher = patch.dict(os.environ, {'HOME': str(self.root)})
        patcher.start()
        self.addCleanup(patcher.stop)

    def test_merge_comments_dedupe_and_canonical_ports(self):
        (self.user / 'lanallow.txt').write_text('# user\n\n10.1.2.3:08080 # web\n100.64.0.1:1\n')
        (self.project / 'lanallow.txt').write_text('10.1.2.3:8080\n192.168.1.2:65535\n')
        config = resolve_config(str(self.project), None)
        self.assertEqual(config.lanallow, [('10.1.2.3', 8080), ('100.64.0.1', 1), ('192.168.1.2', 65535)])
        self.assertIsNone(config.netwhitelist)
        self.assertEqual(resolve_config(None, None).lanallow, [('10.1.2.3', 8080), ('100.64.0.1', 1)])

    def test_empty_and_missing(self):
        self.assertEqual(resolve_config(str(self.project), None).lanallow, [])
        (self.project / 'lanallow.txt').write_text('# empty\n\n')
        self.assertEqual(resolve_config(str(self.project), None).lanallow, [])

    def test_private_boundaries(self):
        addresses = ['10.0.0.0', '10.255.255.255', '172.16.0.0', '172.31.255.255',
                     '192.168.0.0', '192.168.255.255', '169.254.0.0', '169.254.255.255',
                     '100.64.0.0', '100.127.255.255']
        (self.project / 'lanallow.txt').write_text('\n'.join(ip + ':443' for ip in addresses))
        self.assertEqual(resolve_config(str(self.project), None).lanallow, [(ip, 443) for ip in addresses])

    def test_invalid_entries_fail_with_path(self):
        invalid = ['8.8.8.8:443', '127.0.0.1:80', '0.0.0.0:80', '224.0.0.1:80',
                   '172.15.255.255:80', '172.32.0.0:80', '100.63.255.255:80', '100.128.0.0:80',
                   '169.253.1.1:80', '192.169.1.1:80', 'host.local:80', '[::1]:80',
                   '10.1.1.1/32:80', '010.1.1.1:80', '10.1.1.256:80', '10.1.1:80',
                   '10.1.1.1', '10.1.1.1:0', '10.1.1.1:65536', '10.1.1.1:-1',
                   '10.1.1.1:+80', '10.1.1.1:８０', '10.1.1.1:000080',
                   '10.1.1.1: 80', '10.1.1.1:80,10.2.2.2:90', '10.1.1.1:80:90']
        for entry in invalid:
            with self.subTest(entry=entry):
                path = self.project / 'lanallow.txt'
                path.write_text(entry + '\n')
                error = io.StringIO()
                with redirect_stderr(error), self.assertRaises(SystemExit):
                    resolve_config(str(self.project), None)
                self.assertIn(str(path), error.getvalue())


class LanallowRuntimeTests(unittest.TestCase):
    run_container = test_hostports.HostportsTests.run_container

    def test_runtime_handoff_both_modes_and_runtimes(self):
        for runtime in ('docker', 'podman'):
            for restricted in (False, True):
                with self.subTest(runtime=runtime, restricted=restricted):
                    argv = self.run_container(runtime, [], restricted=restricted,
                                              lanallow=[('10.1.2.3', 8080), ('192.168.1.2', 5432)])
                    self.assertEqual(argv.count('CLAUDEBOX_LAN_ALLOW=10.1.2.3:8080,192.168.1.2:5432'), 1)
                    self.assertEqual('CLAUDEBOX_NETRESTRICT=1' in argv, restricted)
                    self.assertFalse(any(arg.startswith('--add-host') for arg in argv))

    def test_combined_and_empty(self):
        argv = self.run_container('docker', [8080], lanallow=[('10.1.2.3', 5432)])
        self.assertIn('CLAUDEBOX_HOST_PORTS=8080', argv)
        self.assertIn('CLAUDEBOX_LAN_ALLOW=10.1.2.3:5432', argv)
        argv = self.run_container('docker', [])
        self.assertFalse(any(arg.startswith('CLAUDEBOX_LAN_ALLOW=') for arg in argv))
