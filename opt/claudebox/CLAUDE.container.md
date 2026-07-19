# claudebox

You are running inside claudebox, a containerized sandbox.

- `/src` is the project root, bind-mounted from the host. `/src/.git` is
  mounted read-only: you can inspect history freely, but commits must be made
  by the user on the host.
- `/home/claude` is per-project persistent state; it survives container exits.
- Check `$CLAUDEBOX_NETWORK`. If it is `restricted`, network egress is limited
  to a whitelist of domains, enforced by a local proxy:
  - `netblocked` lists denied requests; `netblocked -a` shows all proxied
    traffic.
  - The whitelist is the built-in defaults (AI endpoints, read-only package
    hosts, Debian mirrors) plus the project's `.claudebox/netwhitelist.txt`.
    If a host you legitimately need is blocked, ask the user to add it there
    (`host.com` matches exactly; `.host.com` includes subdomains).
  - Tools that ignore `HTTP(S)_PROXY` fail with timeouts or refused
    connections; that is the restriction working, not a service outage.
- To install Debian packages, run `apt-install PACKAGE...` — the only
  permitted sudo command. There is no other root access.
