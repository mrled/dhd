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
    connections unless using an exact LAN TCP allowance below; that is the
    restriction working, not a service outage.
- Unless `$CLAUDEBOX_NETWORK` is `unguarded` (container started by hand
  without root), direct connections into private/link-local address space —
  the container host (`host.docker.internal` / `host.containers.internal`),
  its LAN, tailnets — are blocked by design, apart from DNS to configured
  resolvers and user-configured exceptions, in either network mode:
  - `hostports.txt` allows host gateway aliases on listed TCP ports via Squid only.
  - `lanallow.txt` allows direct TCP and Squid HTTP/CONNECT to explicit
    `IPv4:TCPport` pairs, merged/deduplicated from
    `~/.config/claudebox/lanallow.txt` and the project's `.claudebox/lanallow.txt`
    (blank lines and `#` comments ignored). Ask the user to add needed services;
    `claudebox config add lanallow` creates a commented starter on the host.
    Only `10/8`, `172.16/12`, `192.168/16`, `169.254/16`, and `100.64/10` are
    accepted: public IPs could otherwise bypass the restricted domain whitelist.
    Squid requests must use the literal IP in the HTTP URL or CONNECT authority;
    hostnames resolving to an allowed IP do not qualify for the proxy exception.
    Direct TCP clients may use hostnames resolving to the exact allowed IP.
    For example, `192.168.1.20:445` allows direct SMB to that file server.
    Bypass proxy settings for direct connections if the client honors them.
    No hostnames, CIDRs, IPv6, or port ranges; ports must be 1–65535. Each pair
    is independent, not all combinations of listed IPs and ports.
  - These files export `HTTP(S)_PROXY` for HTTP/CONNECT via Squid without
    activating restriction. Only `lanallow.txt` also allows direct TCP; UDP
    stays blocked. LAN-only allowances do not resolve host aliases at startup.
    Root + NET_ADMIN is required at
    startup whenever allowances are configured (the normal claudebox launch).
- To install Debian packages, run `apt-install PACKAGE...` — the only
  permitted sudo command. There is no other root access.
