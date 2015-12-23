# Marionettist Docker

NOTE: Docker Content Trust requires version 1.8.0 or higher - depending on your system, you may need to upgrade

When creating new images: 

    # 1. Decide on an image name and version. Also set your hub username and tell docker to use Content Trust
    hub_username="mrled"
    image_name="example-docker"
    image_version="0.0.1"
    DOCKER_CONTENT_TRUST=true

    # 2. Create or modify a docker image in the docker/ subdirectory
    emacs docker/$imagea_name/Dockerfile.template
    docker/dmake.py $image_name
    
    # 3. Build it, making sure to tag it as <docker hub username>/<image name>:<image version>
    docker build --tag=$hub_username/$image_name:$image_version build/$image_name/
    
    # 4. Push it
    # If you've never pushed an image protected by Docker Content Trust before, it will ask you for a new root key passphrase
    # If you've never pushed an image to $hub_username/$image_name before, it will ask you for a new repo key passphrase
    # NOTE: if you omit the :$image_version part (what Docker calls the "tag"), Docker will *not* sign your image
    # This is because Docker Content Trust is designed to operate only on tags. If you want to use DCT, you must use tags
    docker push $hub_username/$image_name:$image_version
    
    # 5. Back up your certs and keys
    tar -C ~/.docker/trust/private -c root_keys > ~/root_keys.tar
    # ... now encrypt that backup and delete the tarfile ...
    # Note that this just gets your DCT root key - not your repo keys. 
    # (See <https://docs.docker.com/engine/security/trust/trust_key_mng/>)
    # To get your repo keys, back up .docker/trust/private/tuf_keys in addition to root_keys
    
When consuming images: 

    # 1. Determine image name and version and hub username and use Content Trust
    hub_username="mrled"
    image_name="example-docker"
    image_version="0.0.1"
    DOCKER_CONTENT_TRUST=true

    # 2. Pull the image
    # NOTE: this does NOT ASK YOU if you want to trust it -- it AUTOMATICALLY trusts the first fucking key it's given. Christ.
    docker pull $hub_username/$image_name
    
NOTES: 

- If you can't figure out why it's not trying to use Trusted Content, you can force it with `docker <COMMAND> --disable-content-trust=false` (lol yes, that seems backwards to me too)
- To tag something twice, you can *build* it one time, then `docker tag` the image that you build, and then you have to `docker push` both tags separately (you have to call `docker push` twice)
- dmake can handle pushing after the keys are already set up locally, but currently, you have to do your initial `docker push` by calling docker from your shell so it can interactively ask you for the fucking passwords. christ.    

## Using docker-machine

Initially, I was planning to use Ansible to build the Docker images and push them to a local registry, but I abandoned that when I discovered that you can sign your Docker images (Docker calls this "Docker Content Trust") and push them to the official registry, which hosts them for free. Damn, nice. 

(One note about this: currently, docker automatically trusts the first key that it sees to have signed a given image. In the future, the Notary team will support disabling this trust (I opened [a bug](https://github.com/docker/notary/issues/342) for this), and requiring that certificates get distributed out of band first. Clearly, it's preferable to switch to this mode when it is available.)

After that, my plan was to build, sign, and push the images locally, and rely on Ansible to deploy them. However, unfortunately, Ansible does not have support for Content Trust, so I can't use them both, leading me to...

Now, I have decided to build, sign, and push Docker images locally, and also deploy them using `docker-machine`, which will let you interact with Docker as if it were running locally, which actually doing builds on a different machine. Seemingly, this was designed for OS X, Windows, and other non-Linux users who want to use Docker as if it were installed on their host OS, but there is a connector (which Docker Machine calls a "driver") for [generic](https://docs.docker.com/machine/drivers/generic/) machines - that is, any Linux machine that supports Docker that you have SSH access to.

NOTE: you must currently use the IP address to talk to the machine - it is NOT accessible via DNS. lol. 

With this, I can deploy to a Docker host from any computer, without having to create an SSH session - I can act as if Docker is working entirely locally, but actually it's on my Docker server elsewhere on the Internet. Nice. 

    # Determine IP address, SSH port, username, and SSH key to use
    dockerip=127.0.0.1
    dockermachinename=dockervm
    # You may be able to take the defaults for these, and not specify them on the command line: 
    #dockeruser=root
    #dockersshport=22
    #dockersshkey=~/.ssh/id_rsa

    # Create the docker-machine configuration on the local machine (just do this once)
    docker-machine create --driver generic --generic-ip-address $dockerip $dockermachinename

    # Configure your current environment to connect to your docker machine (do this for each shell)
    eval "$(docker-machine env nullyork)"

