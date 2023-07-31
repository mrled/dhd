# This BREAKS system-setup, AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
# https://indeed-pte.slack.com/archives/CHUMDDT1U/p1647987796916879
unset DOCKER_SCAN_SUGGEST

# See also <https://code.corp.indeed.com/sre/sre-acceleration/decommissioner>
decommissioner_export() {
    export GITLAB_TOKEN="$(gopass cat mledbetter/gitlab_token)"
    export LDAP_USERNAME=mledbetter
    export LDAP_PASSWORD="$(gopass cat mledbetter/ldap_password)"
    export JENKINS_TOKEN="$(gopass cat mledbetter/jenkins_token)"
    export DD_API_KEY="$(gopass cat indeed/datadog_api_key_sre)"
    export DD_APP_KEY="$(gopass cat mledbetter/datadog_app_key)"
}

# This is for prod; see also <https://wiki.indeed.com/display/ITSYS/Vault>
export VAULT_ADDR='https://vault.indeed.tech'




