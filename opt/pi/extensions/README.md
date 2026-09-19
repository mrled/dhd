# dhd pi extensions

Add the bundle entry point at `index.ts`.
Claudebox mounts this directory read-only at:

```text
~/.pi/agent/extensions/dhd
```

Pi automatically loads `extensions/*/index.ts`,
so the entry point is active in every standard claudebox run.

Edit extensions on the host, and `/reload` (or restart Pi altogether) to load changes.
