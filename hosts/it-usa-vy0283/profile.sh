## My own customizations:

# Setting a more restrictive umask breaks system-setup
umask 022

## Must run system-setup, then cut the parts between these two lines and paste them below
##   BEGIN env Setup -- Managed by Ansible DO NOT EDIT.
##   END env Setup -- Managed by Ansible DO NOT EDIT.

# Homebrew uses a /opt/homebrew on Apple Silicon.
eval $(/opt/homebrew/bin/brew shellenv)

# Setup INDEED_ENV_DIR earlier.
if [ -z "${INDEED_ENV_DIR}" ]; then
    export INDEED_ENV_DIR="/Users/mledbetter/env"
fi

# Single-brace syntax because this is required in bash, dash, zsh, etc
if [ -e "${INDEED_ENV_DIR}/etc/indeed_profile" ]; then
    . "${INDEED_ENV_DIR}/etc/indeed_profile"
fi

# Add ~/env/bin to your PATH to use the shared shell scripts from delivery/env
if [ -d "${INDEED_ENV_DIR}/bin" ]; then
    PATH="${INDEED_ENV_DIR}/bin:$PATH"
fi

# On OSX, explicitly source .bashrc so that OS X bash is guaranteed to include all definitions, changing .bashrc to .zshrc for zsh
if [ "Darwin" = "$(uname -s)" ]; then
    [ -s "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi
