# dreadnaught CLAUDE.md

- `claude` is running in a virtual machine and has full access to a regular user account there.
  Run any command, or see any file on the filesystem that a normal user would have access to.
- Work is in `/mnt/aiworkspace`.
- You have a gitea account on <https://gitea.dreadnaught.backchannel.younix.us>
  - SSH to that git host via `gitea@gitea.dreadnaught.backchannel.younix.us:2222`
  - SSH is configured so that `dreadgit` is an alias for the above (e.g. `ssh dreadgit` should just work)
  - Access to the `tea` command line, which should already be logged in
- Work like this
  - On dreadgit, the `mirror` org contains repositories mirrored from elsewhere
  - The repos are named `SOURCE--OWNER--REPO` like `github--mrled--dhd`
  - These repos are automatically kept up to date with the upstream repo
  - You can read them but not commit to them
  - When asked to change them, use your own fork of the repo, make the change there, and issue a pull request
  - Check out repos to `/mnt/aiworkspace`
  - Many repos are already checked out there --- check that directory first before making a new clone
- You can put any port listening on localhost behind a reverse proxy with real HTTPS
  - Drop files in `/etc/chineseroom/domainmap.sandbox.d/`
  - Format: `SUBDOMAIN PORT`, one per line
  - Can have multiple files in that directory, and each file can have multiple entries
  - Check before adding a new one, make sure that it isn't already in use (port or subdomain)
  - After making changes, you must run `sudo /usr/local/bin/regen-nginx-mappings.sh` (no password required)
  - These are served at `https://SUBDOMAIN.sandbox.dreadnaught.backchannel.younix.us`
  - If you need to create a new service, you should create a systemd user unit for it that listens on an unused port on localhost
