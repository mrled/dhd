# Marionettiset Ansible

Controlling other machines with puppet strings

## How to use

The design goals are: *All of my servers should be configured on a local, securable workstation, and all data should be checked into Git*

1. All servers should be configured on a local, securable workstation
2. Ansible endpoints and Docker containers should not be the source of truth for their own data
    - Non-secret configurations should be checked into Git
    - Secret configurations should be ... handled... somehow?? ?
    - Backups should be encrypted with my automatic backup GPG cert and shipped off to S3...
    - ... but try to avoid backing things up if all you need to do is manage configuration! 

This means I want to use the following workflow: 

- Build your Docker containers out of band - do NOT build them in Ansible
    - This is not how I've build my roles so far: 
        - apache
        - discourse-sever
        - ghost-server
        - openvpn-server
    - But it *is* how I want to build them going forward
- Push your Docker containers to the hub.
    - *Use content trust* by setting the `DOCKER_CONTENT_TRUST` environment variable before doing a docker push/pull/whatever
    - This means that I sign my docker images, and I don't have to trust Docker's infrastructure
- Use Ansible to deploy the Docker containers on the host, and obviously for any other configuration

## Ansible and Docker

I think it's time to say that my Docker hosts will not be configured by Ansible, at least for the time being

- Ansible doesn't support Docker Content Trust at this time
- Docker has good support for remote work with docker-machine
- 