"""Entrypoint hostport regressions without root, networking, or a runtime.

Run with: python3 -m unittest discover -s opt/claudebox/tests -v
"""

from pathlib import Path
import subprocess
import tempfile
import unittest


CLAUDEBOX = Path(__file__).resolve().parents[1]
ENTRYPOINT = CLAUDEBOX / "entrypoint.sh"

# All commands that could change privileges, firewall state, ownership, or
# access the network are intercepted, even when the tests themselves run as root.
STUB = """#!/bin/sh
command=${0##*/}
printf '%s %s\\n' "$command" "$*" >> "$TEST_CAPTURE/commands"
case "$command" in
    id)
        case "$*" in
            '-u') printf '0\\n' ;;
            '-u proxy') printf '731\\n' ;;
            *) exit 90 ;;
        esac ;;
    getent)
        [ "$1" = ahostsv4 ] || exit 91
        case "$2" in
            host.docker.internal) address=$TEST_DOCKER_IP ;;
            host.containers.internal) address=$TEST_PODMAN_IP ;;
            *) exit 92 ;;
        esac
        [ -n "$address" ] || exit 2
        printf '%s STREAM %s\\n%s DGRAM %s\\n' \\
            "$address" "$2" "$address" "$2" ;;
    nft) cat > "$TEST_CAPTURE/nft.rules" ;;
    squid) printf '%s\\n' "$*" > "$TEST_CAPTURE/squid.args" ;;
    curl|chown) exit 0 ;;
    setpriv)
        printf '%s\\n' "$*" > "$TEST_CAPTURE/setpriv.args"
        printf '%s\\n' "$CLAUDEBOX_NETWORK" "$HTTP_PROXY" "$HTTPS_PROXY" \\
            "$NO_PROXY" > "$TEST_CAPTURE/environment" ;;
    *) exit 93 ;;
esac
"""


class EntrypointHostportsTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="entrypoint-hostports-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.run_dir = self.root / "run"
        self.etc_dir = self.root / "etc"
        self.log_dir = self.root / "logs"
        self.capture = self.root / "capture"
        self.bin_dir = self.root / "bin"
        for directory in (self.run_dir, self.etc_dir, self.capture, self.bin_dir):
            directory.mkdir()
        self.resolver = self.root / "resolv.conf"
        self.resolver.write_text("nameserver 10.0.0.53\nnameserver fd00::53\n")
        self.replacements = {
            "/run/claudebox": str(self.run_dir),
            "/etc/claudebox": str(self.etc_dir),
            "/var/log/squid": str(self.log_dir),
            "/etc/resolv.conf": str(self.resolver),
        }
        self.script = self.root / "entrypoint.sh"
        self.script.write_text(self.redirect(ENTRYPOINT.read_text()))
        (self.etc_dir / "squid.conf").write_text(
            self.redirect((CLAUDEBOX / "squid.conf").read_text())
        )
        (self.etc_dir / "netwhitelist-default.txt").write_text(
            "# fixture\n.example.org\n"
        )
        for command in ("id", "getent", "nft", "squid", "curl", "setpriv", "chown"):
            stub = self.bin_dir / command
            stub.write_text(STUB)
            stub.chmod(0o755)

    def redirect(self, text):
        for original, replacement in self.replacements.items():
            text = text.replace(original, replacement)
        return text

    def run_entrypoint(self, ports="8080,5432", docker="172.17.0.1", podman="", restricted=False):
        # Do not inherit CLAUDEBOX_* settings or proxy settings from the caller.
        environment = {
            "PATH": f"{self.bin_dir}:/usr/bin:/bin",
            "LC_ALL": "C",
            "TMPDIR": str(self.root),
            "CLAUDEBOX_HOST_PORTS": ports,
            "CLAUDEBOX_NETRESTRICT": "1" if restricted else "",
            "TEST_CAPTURE": str(self.capture),
            "TEST_DOCKER_IP": docker,
            "TEST_PODMAN_IP": podman,
        }
        return subprocess.run(
            ["/bin/sh", str(self.script), "test-command"],
            env=environment,
            capture_output=True,
            text=True,
            timeout=10,
        )

    def assert_policy(self, address, ports=(8080, 5432), restricted=False):
        self.assertEqual(
            (self.run_dir / "host-service-hosts").read_text(),
            f"{address} host.docker.internal host.containers.internal\n",
        )
        if restricted:
            self.assertEqual((self.run_dir / "whitelist.txt").read_text().splitlines(), [".example.org"])
        else:
            self.assertFalse((self.run_dir / "whitelist.txt").exists())
        rules = (self.capture / "nft.rules").read_text()
        host_rule = (
            f"meta skuid 731 ip daddr {address} tcp dport "
            f"{{ {', '.join(map(str, ports))} }} accept"
        )
        self.assertEqual(rules.count(host_rule), 1)
        self.assertIn("policy drop;" if restricted else "policy accept;", rules)
        self.assertIn("icmpv6 type { nd-router-solicit, nd-neighbor-solicit, nd-neighbor-advert } accept", rules)
        self.assertNotIn(f"ip daddr {address} udp", rules)
        # No host accept without the proxy uid, nor a broad host-IP exception.
        host_lines = [line.strip() for line in rules.splitlines() if address in line]
        self.assertEqual(host_lines, [host_rule])
        prefix = "meta skuid 731 " if restricted else ""
        for private_drop in (prefix + "ip daddr {", prefix + "ip6 daddr {"):
            self.assertLess(rules.index(host_rule), rules.index(private_drop))
            if restricted:
                self.assertLess(rules.index(private_drop), rules.index("meta skuid 731 accept"))
        self.assertIn(prefix + "ip daddr 10.0.0.53 udp dport 53 accept", rules)
        self.assertIn(prefix + "ip6 daddr fd00::53 tcp dport 53 accept", rules)

        config = (self.run_dir / "squid.conf").read_text()
        self.assertEqual(
            config.splitlines()[0], f"hosts_file {self.run_dir}/host-service-hosts"
        )
        self.assertIn(
            "acl host_service_host dstdomain host.docker.internal host.containers.internal\n",
            config,
        )
        port_acl = next(
            line for line in config.splitlines()
            if line.startswith("acl host_service_port ")
        )
        self.assertEqual(
            port_acl.split(), ["acl", "host_service_port", "port", *map(str, ports)]
        )
        allow = "http_access allow host_service_host host_service_port"
        self.assertEqual(config.count(allow), 1)
        public_allow = "http_access allow whitelisted" if restricted else "http_access allow all"
        for deny in ("http_access deny host_service_host", "http_access deny to_private"):
            self.assertLess(config.index(allow), config.index(deny))
            self.assertLess(config.index(deny), config.index(public_allow))
        if restricted:
            self.assertLess(config.index(allow), config.index("http_access deny CONNECT !SSL_ports"))
        else:
            self.assertNotIn("http_access deny CONNECT !SSL_ports", config)
            self.assertNotIn("acl whitelisted", config)
        self.assertEqual(
            (self.capture / "squid.args").read_text(), f"-f {self.run_dir}/squid.conf\n"
        )
        self.assertEqual(
            (self.capture / "environment").read_text().splitlines(),
            ["restricted" if restricted else "unrestricted", "http://127.0.0.1:3128", "http://127.0.0.1:3128", "localhost,127.0.0.1"],
        )
        self.assertIn(
            "--reuid=claude --regid=claude --init-groups env HOME=/home/claude "
            "USER=claude LOGNAME=claude test-command",
            (self.capture / "setpriv.args").read_text(),
        )

    def test_docker_resolution_pins_both_aliases_and_narrow_policy(self):
        result = self.run_entrypoint(podman="10.88.0.1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_policy("172.17.0.1")
        commands = (self.capture / "commands").read_text()
        self.assertIn("getent ahostsv4 host.docker.internal\n", commands)
        self.assertNotIn("getent ahostsv4 host.containers.internal", commands)

    def test_podman_resolution_falls_back_and_pins_both_aliases(self):
        result = self.run_entrypoint(docker="", podman="10.88.0.1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_policy("10.88.0.1")
        commands = (self.capture / "commands").read_text()
        self.assertLess(
            commands.index("getent ahostsv4 host.docker.internal"),
            commands.index("getent ahostsv4 host.containers.internal"),
        )

    def test_explicit_restriction_preserves_whitelist_and_connect_policy(self):
        result = self.run_entrypoint(restricted=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_policy("172.17.0.1", restricted=True)

    def test_user_whitelist_enables_restriction(self):
        (self.run_dir / "netwhitelist.txt").write_text("# user fixture\n")
        result = self.run_entrypoint()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_policy("172.17.0.1", restricted=True)

    def test_public_host_address_still_denies_unlisted_alias_ports(self):
        result = self.run_entrypoint(docker="203.0.113.1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_policy("203.0.113.1")

    def test_no_hostports_smoke(self):
        for restricted in (False, True):
            with self.subTest(restricted=restricted):
                result = self.run_entrypoint(ports="", restricted=restricted)
                self.assertEqual(result.returncode, 0, result.stderr)
                rules = (self.capture / "nft.rules").read_text()
                self.assertIn("policy drop;" if restricted else "policy accept;", rules)
                self.assertIn("nd-neighbor-solicit", rules)
                self.assertNotIn("tcp dport {", rules)
                environment = (self.capture / "environment").read_text().splitlines()
                self.assertEqual(environment[0], "restricted" if restricted else "unrestricted")
                self.assertEqual(environment[1], "http://127.0.0.1:3128" if restricted else "")

    def assert_failed_closed(self, result):
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(list(self.run_dir.iterdir()), [])
        self.assertFalse(self.log_dir.exists())
        commands = (self.capture / "commands").read_text().splitlines()
        self.assertFalse(any(
            line.split()[0] in ("nft", "squid", "curl", "setpriv", "chown")
            for line in commands
        ))
        self.assertEqual(
            sorted(path.name for path in self.capture.iterdir()), ["commands"]
        )

    def test_resolution_failure_fails_closed_before_firewall_proxy_or_exec(self):
        result = self.run_entrypoint(docker="", podman="")
        self.assert_failed_closed(result)
        self.assertIn("cannot resolve an IPv4 container host address", result.stderr)
        commands = (self.capture / "commands").read_text()
        self.assertIn("getent ahostsv4 host.docker.internal", commands)
        self.assertIn("getent ahostsv4 host.containers.internal", commands)

    def test_invalid_ports_fail_closed_before_resolution(self):
        for ports in (
            "0", "65536", "123456", "-1", "abc", "80 443", "80\n443",
            ",80", "80,", "80,,443", "80,0", "8080,65536",
        ):
            with self.subTest(ports=ports):
                result = self.run_entrypoint(ports=ports)
                self.assert_failed_closed(result)
                self.assertIn("invalid host port", result.stderr)
                self.assertNotIn("getent", (self.capture / "commands").read_text())

    def test_valid_port_boundaries_are_kept_narrow(self):
        result = self.run_entrypoint(ports="1,65535")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_policy("172.17.0.1", ports=(1, 65535))


if __name__ == "__main__":
    unittest.main()
