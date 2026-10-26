# Writing shell

- Unless specifically requested, do not write bash scripts or use bashism, but write only POSIX sh scripts
- If functionality cannot be handled well by a POSIX sh script, suggest another language like Python instead of a more featureful shell like bash
- Use `test` instead of `[` and `]`
- Always use `set -eu`, and properly handle uninitialized variables
