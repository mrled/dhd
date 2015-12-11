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
    
