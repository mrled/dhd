## My own customizations:


# This BREAKS system-setup, AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
# https://indeed-pte.slack.com/archives/CHUMDDT1U/p1647987796916879
unset DOCKER_SCAN_SUGGEST


## Must run system-setup, then cut the parts between these two lines and paste them below
##   BEGIN env Setup -- Managed by Ansible DO NOT EDIT.
##   END env Setup -- Managed by Ansible DO NOT EDIT.

# Setup INDEED_ENV_DIR earlier.
if [ -z "${INDEED_ENV_DIR}" ]; then
    export INDEED_ENV_DIR="/Users/mledbetter/env"
fi

# Single-brace syntax because this is required in bash and sh alike
if [ -e "${INDEED_ENV_DIR}/etc/indeedrc" ]; then
    . "${INDEED_ENV_DIR}/etc/indeedrc"
fi
