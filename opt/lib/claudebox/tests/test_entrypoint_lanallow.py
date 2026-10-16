"""LAN-only and combined entrypoint policy generation, with stubbed commands."""
import shutil
import socket
import subprocess
import time
import unittest
import test_entrypoint_hostports


class EntrypointLanallowTests(unittest.TestCase):
    setUp = test_entrypoint_hostports.EntrypointHostportsTests.setUp
    redirect = test_entrypoint_hostports.EntrypointHostportsTests.redirect
    run_entrypoint = test_entrypoint_hostports.EntrypointHostportsTests.run_entrypoint
    assert_failed_closed = test_entrypoint_hostports.EntrypointHostportsTests.assert_failed_closed

    def test_paired_rules_in_both_modes(self):
        for restricted in (False, True):
            with self.subTest(restricted=restricted):
                baseline = self.run_entrypoint(ports='', restricted=restricted)
                self.assertEqual(baseline.returncode, 0, baseline.stderr)
                baseline_rules = (self.capture / 'nft.rules').read_text()
                baseline_accepts = [line.strip() for line in baseline_rules.splitlines()
                                    if line.strip().endswith(' accept')]
                result = self.run_entrypoint(ports='', lanallow='10.1.2.3:8080,192.168.1.2:5432', restricted=restricted)
                self.assertEqual(result.returncode, 0, result.stderr)
                rules = (self.capture / 'nft.rules').read_text()
                config = (self.run_dir / 'squid.conf').read_text()
                self.assertIn('policy drop;' if restricted else 'policy accept;', rules)
                expected = []
                for index, (ip, port) in enumerate([('10.1.2.3', 8080), ('192.168.1.2', 5432)]):
                    rule = f'ip daddr {ip} tcp dport {port} accept'
                    expected.append(rule)
                    for private_drop in ('ip daddr {', 'ip6 daddr {'):
                        self.assertLess(rules.index(rule), rules.index(private_drop))
                    if restricted:
                        self.assertLess(rules.index(rule), rules.index('meta skuid 731 accept'))
                    self.assertIn(f'acl lan_service_{index}_literal dstdomain -n {ip}\n', config)
                    self.assertIn(f'acl lan_service_{index}_dst dst {ip}/32\n', config)
                    self.assertIn(f'acl lan_service_{index}_port port {port}\n', config)
                    allow = f'http_access allow lan_service_{index}_literal lan_service_{index}_dst lan_service_{index}_port'
                    self.assertEqual(config.count(allow), 1)
                    self.assertLess(config.index(allow), config.index('http_access deny to_private'))
                    if restricted:
                        self.assertLess(config.index(allow), config.index('http_access deny CONNECT !SSL_ports'))
                actual = [line.strip() for line in rules.splitlines() if '10.1.2.3' in line or '192.168.1.2' in line]
                self.assertEqual(actual, expected)
                # Exact, UID-independent TCP pairs: no Cartesian sets, UDP,
                # broad workload-user accept, or other new egress allowances.
                accepts = [line.strip() for line in rules.splitlines() if line.strip().endswith(' accept')]
                self.assertEqual([line for line in accepts if line not in expected], baseline_accepts)
                self.assertEqual([line for line in rules.splitlines()
                                  if line.strip() and line.strip() not in expected],
                                 [line for line in baseline_rules.splitlines() if line.strip()])
                if restricted:
                    self.assertIn('meta skuid 731 ip daddr {', rules)
                    self.assertIn('meta skuid 731 ip6 daddr {', rules)
                    self.assertLess(rules.index('ip6 daddr {'), rules.index('meta skuid 731 accept'))
                lan_lines = [line for line in config.splitlines() if 'lan_service_' in line]
                self.assertEqual(lan_lines, [
                    'acl lan_service_0_literal dstdomain -n 10.1.2.3',
                    'acl lan_service_0_dst dst 10.1.2.3/32',
                    'acl lan_service_0_port port 8080',
                    'http_access allow lan_service_0_literal lan_service_0_dst lan_service_0_port',
                    'acl lan_service_1_literal dstdomain -n 192.168.1.2',
                    'acl lan_service_1_dst dst 192.168.1.2/32',
                    'acl lan_service_1_port port 5432',
                    'http_access allow lan_service_1_literal lan_service_1_dst lan_service_1_port',
                ])
                self.assertNotIn('hosts_file', config)
                self.assertNotIn('getent', (self.capture / 'commands').read_text())
                self.assertIn('http_access allow whitelisted' if restricted else 'http_access allow all', config)
                self.assertEqual('acl whitelisted' in config, restricted)
                env = (self.capture / 'environment').read_text().splitlines()
                self.assertEqual(env[0], 'restricted' if restricted else 'unrestricted')
                self.assertEqual(env[1:3], ['http://127.0.0.1:3128'] * 2)

    def test_combined_host_and_lan(self):
        result = self.run_entrypoint(lanallow='100.64.0.1:443', restricted=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        rules = (self.capture / 'nft.rules').read_text()
        self.assertIn('\n        ip daddr 100.64.0.1 tcp dport 443 accept\n', rules)
        self.assertIn('meta skuid 731 ip daddr 172.17.0.1 tcp dport { 8080, 5432 } accept', rules)
        config = (self.run_dir / 'squid.conf').read_text()
        self.assertIn('hosts_file', config)
        self.assertIn('http_access allow host_service_host host_service_port', config)
        self.assertIn('http_access allow lan_service_0_literal lan_service_0_dst lan_service_0_port', config)

    def test_whitelist_file_still_restricts(self):
        (self.run_dir / 'netwhitelist.txt').write_text('.internal.example\n')
        result = self.run_entrypoint(ports='', lanallow='169.254.1.2:80')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('policy drop;', (self.capture / 'nft.rules').read_text())
        self.assertIn('http_access allow whitelisted', (self.run_dir / 'squid.conf').read_text())

    def test_nonroot_refused(self):
        result = self.run_entrypoint(ports='', lanallow='10.1.2.3:80', uid='1000')
        self.assert_failed_closed(result)
        self.assertIn('requires starting as root', result.stderr)

    def test_malformed_env_fails_before_rules_or_resolution(self):
        invalid = ['10.1.2.3', ':80', '10.1.2.3:', '10.1.2.3:0', '10.1.2.3:65536',
                   '10.1.2.3:000080', '10.1.2.3:-1', '10.1.2.3:８０',
                   '10.1.2.3:80,', ',10.1.2.3:80', '10.1.2.3:80,,10.2.3.4:90',
                   '10.1.2.3:80\n', '10.1.2.3:80 # comment', '10.1.2.3:80; accept',
                   '10.1.2.3:80\nip daddr 0.0.0.0/0 accept', '10.1.2.3/32:80',
                   '010.1.2.3:80', '10.1.2.256:80', '10.1.2:80', 'host.local:80',
                   '[::1]:80', '8.8.8.8:443', '127.0.0.1:80', '172.32.0.0:80',
                   '100.128.0.0:80', '169.255.0.0:80', '192.169.0.0:80',
                   '10.1.2.3:80,8.8.8.8:443', '10.1.2.3:80,10.2.3.4:0']
        for value in invalid:
            with self.subTest(value=value):
                result = self.run_entrypoint(lanallow=value, restricted=True)
                self.assert_failed_closed(result)
                self.assertIn('invalid LAN allowance', result.stderr)
                self.assertNotIn('getent', (self.capture / 'commands').read_text())

    def test_duplicate_env_pairs_are_canonicalized(self):
        result = self.run_entrypoint(ports='', lanallow='10.1.2.3:08080,10.1.2.3:8080,10.1.2.3:443')
        self.assertEqual(result.returncode, 0, result.stderr)
        rules = (self.capture / 'nft.rules').read_text()
        self.assertEqual(rules.count('ip daddr 10.1.2.3 tcp dport 8080 accept'), 1)
        self.assertEqual(rules.count('ip daddr 10.1.2.3 tcp dport 443 accept'), 1)
        config = (self.run_dir / 'squid.conf').read_text()
        self.assertEqual(config.count('http_access allow lan_service_'), 2)

    @unittest.skipUnless(shutil.which('squid'), 'Squid not installed')
    def test_generated_config_parses_in_real_squid(self):
        for ports in ('', '8080,5432'):
            for restricted in (False, True):
                with self.subTest(ports=ports, restricted=restricted):
                    result = self.run_entrypoint(ports=ports, restricted=restricted,
                                                 lanallow='10.1.2.3:8080,192.168.1.2:5432')
                    self.assertEqual(result.returncode, 0, result.stderr)
                    result = subprocess.run(
                        [shutil.which('squid'), '-k', 'parse', '-f', str(self.run_dir / 'squid.conf')],
                        capture_output=True, text=True, timeout=10,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)

    @unittest.skipUnless(shutil.which('squid'), 'Squid not installed')
    def test_real_squid_lan_acl_requires_literal_ip(self):
        result = self.run_entrypoint(ports='', restricted=True, lanallow='10.1.2.3:8080')
        self.assertEqual(result.returncode, 0, result.stderr)
        generated = (self.run_dir / 'squid.conf').read_text()
        # Exercise the actual generated conjunction without making upstream
        # connections: matching the allowance returns 409, everything else 403.
        lan_rules = '\n'.join(line.replace('http_access allow', 'http_access deny')
                              for line in generated.splitlines() if 'lan_service_' in line)
        hosts = self.root / 'acl-hosts'
        hosts.write_text('10.1.2.3 private.test mixed.test\n203.0.113.1 mixed.test\n')
        with socket.socket() as listener:
            listener.bind(('127.0.0.1', 0))
            port = listener.getsockname()[1]
        config = self.root / 'acl-test.conf'
        config.write_text(
            f'http_port 127.0.0.1:{port}\nvisible_hostname acl-test\n'
            f'pid_filename {self.root}/acl-test.pid\n'
            f'cache_log {self.root}/acl-test.log\naccess_log none\n'
            f'hosts_file {hosts}\n'
            'deny_info 409:ERR_ACCESS_DENIED lan_service_0_port\n'
            + lan_rules + '\nhttp_access deny all\n'
        )
        diagnostics = (self.root / 'acl-test.stderr').open('w+')
        self.addCleanup(diagnostics.close)
        process = subprocess.Popen([shutil.which('squid'), '-N', '-n', f'lanacl{port}', '-f', str(config)],
                                   stdout=subprocess.DEVNULL, stderr=diagnostics)
        def stop():
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)
        self.addCleanup(stop)
        for attempt in range(100):
            try:
                with socket.create_connection(('127.0.0.1', port), timeout=0.2):
                    break
            except OSError:
                if process.poll() is not None:
                    diagnostics.seek(0)
                    log = self.root / 'acl-test.log'
                    self.fail('Squid exited during startup: ' + diagnostics.read()
                              + (log.read_text() if log.exists() else ''))
                time.sleep(0.05)
        else:
            self.fail('Squid did not listen')
        for method in ('GET', 'CONNECT'):
            for host, target_port, expected in (
                ('10.1.2.3', 8080, 409),
                ('10.1.2.3', 8081, 403),
                ('10.1.2.4', 8080, 403),
                ('private.test', 8080, 403),
                ('mixed.test', 8080, 403),
            ):
                with self.subTest(method=method, host=host, port=target_port):
                    authority = f'{host}:{target_port}'
                    target = authority if method == 'CONNECT' else f'http://{authority}/'
                    with socket.create_connection(('127.0.0.1', port), timeout=2) as client:
                        client.settimeout(5)
                        client.sendall(f'{method} {target} HTTP/1.1\r\nHost: {authority}\r\nConnection: close\r\n\r\n'.encode())
                        response = client.makefile('rb').readline()
                    self.assertEqual(int(response.split()[1]), expected, response)

    def test_private_range_and_port_boundaries(self):
        ips = ['10.0.0.0', '10.255.255.255', '172.16.0.0', '172.31.255.255',
               '192.168.0.0', '192.168.255.255', '169.254.0.0', '169.254.255.255',
               '100.64.0.0', '100.127.255.255']
        result = self.run_entrypoint(ports='', lanallow=','.join(ip + ':65535' for ip in ips) + ',10.1.1.1:00001')
        self.assertEqual(result.returncode, 0, result.stderr)
        rules = (self.capture / 'nft.rules').read_text()
        for ip in ips:
            self.assertIn(f'\n        ip daddr {ip} tcp dport 65535 accept\n', rules)
        self.assertIn('\n        ip daddr 10.1.1.1 tcp dport 1 accept\n', rules)
