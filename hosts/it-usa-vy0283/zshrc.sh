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
    export DATA_WORLD_TOKEN=""
}

# For running 'terraform plan' locally
tfplan_export() {
    export DATADOG_API_KEY="$(gopass cat indeed/datadog_api_key_sre)"
    export DATADOG_APP_KEY="$(gopass cat mledbetter/datadog_app_key)"
}

# For AI Tools CLI
aitools_export() {
    export INDEED_LLM_PROXY_API_KEY="$(gopass cat mledbetter/llm-proxy-prod)"
}

# This is for prod; see also <https://wiki.indeed.com/display/ITSYS/Vault>
export VAULT_ADDR='https://vault.indeed.tech'




