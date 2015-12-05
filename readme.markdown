# marionettist readme

Controlling other machines with puppet strings

## How to use

The design goals are: *All of my servers should be configured on a local, securable workstation, and all data should be checked into Git*

1. All servers should be configured on a local, securable workstation
2. All data should be checked in to Git; there should be no assumption that any server is backed up

For some apps, the second goal won't make sense. For example, a database of user comments on a blog isn't easily commitable to Git (unless you're using a blogging system that commits them for you). However, by making this the rule of thumb, the exceptions can be clearly laid out and a backup strategy put in place.

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

### How to use Docker

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
    docker push $hub_username/$image_name
    
When consuming images: 

    # 1. Determine image name and version and hub username and use Content Trust
    hub_username="mrled"
    image_name="example-docker"
    image_version="0.0.1"
    DOCKER_CONTENT_TRUST=true

    # 2. Pull the image
    docker pull $hub_username/$image_name
    
REMAINING QUESTIONS: 

- How do I tag something with more than one tag - like if I want to have a :latest tag or something?

## devlog

### 2015-11-29: thoughts on docker and configuration management

Docker + Ansible is a little awkward. 

- You have to create a container for your app, AND a container for the app's data
    - Why a data container? Host volumes introduce frustrating permission problems. 
        - You want your Docker processes to NOT run as root
        - But there's a separate /etc/passwd in each Docker instance, so separate users
        - Users are NOT shared with the host... but UIDs are
        - If you use host volumes, you end up with shit in your host volume directory that has permissions you don't want to have to manage... you'd rather manage them inside the Docker container
        - With Ansible, it's worse, because you cannot tell it what permissions to use when copying files unless you tightly couple your Ansible configuration stuff with the exact machine... not ideal
- Even so, the Ansible playbook ends up looking like this:
    - Make data directory (on the Docker host)
    - Upload file asdf.txt to the host data directory
    - Get the name of the Docker data container... somehow...
    - Copy asdf.txt to a temporary folder on the Docker data container using `docker cp`
    - Change the permissions of asdf.txt and move it into the final place using `docker exec` 

Is there a better way to handle that? 

- Other people have run into this:
    - <https://github.com/docker/docker/issues/2841>
- I could NOT use Ansible to handle configuration of apps in Docker, and copy files to them directly
    - This is not a good solution because it means my configuration could get out of sync with marionettist if I forget to update them 
- I could add an SSH server to all of my Docker containers and configure the files that way
    - Not ideal because who wants an SSH server on their Docker containers tbh
    - I don't need to use Docker data containers for this; the app containers could then keep all their data internally because none of it is persistent (from Docker's perspective - the persistence is in marionettist/Ansible)
- I could configure my Docker data containers to pull down files from a Git repository somehow
    - This keeps my marionettist design goal of being able to do everything noninteractively from a workstation
    - However, I can't really use public repos (free GH account, etc)... what about the fairly common case of database creds (etc) in config files?
    - Also, how would I separate permissions from the different containers? Separate repo for each data container? Sounds like a lot of management, ok, what if I create Ansible scripts to manage my repos... lol
- DockerDNA is a Docker image base (...? or something?) that is supposed to fix this
    - It is very simple - I could implement it myself, I don't have to use their GH repo
    - It runs `ansible-playbook` in the Dockerfile
    - So the workflow would be: edit a file, regen the Docker data image, upload to 
- Wait. **What if I just did nothing?**
    - Keep using host volumes with Docker
    - Don't worry about UID collisions because *no service (except SSH) should run on the Docker host*
        - This means that it's very hard for malicious code in data used by a Docker container to infect another container/the host, because (even if, say, the config file is JavaScript source code or something) it will never get run outside of that container ANYWAY
        - Then, the only time I'd HAVE to use a Docker data container is if I wanted to share data between containers... which at least right now I don't have to do at all
    - Still the problem is what UID do I give to Ansible when it uploads the file? That's the main problem 
    - I could reserve some UIDs on the Docker host by creating ~10 or so UIDs called like `DOCKER-00` through `DOCKER-09`, and using those UIDs inside each Dockerfile
    - I could even use the same userNAMEs when creating users inside the Dockerfile. I could even use the same script! 
    - This reduces portability
        - ... at least, it does require configuration of the Docker host
        - But my goal is to have my Docker host be doing nothing but Docker anyway
        - It also makes it impossible for me to use externally defined Docker images without rewriting them
            - But I already don't do that
            - The Docker community, like all communities, is completely untrustworthy when it comes to security
            - Almost every Dockerfile I see on the web runs its application as fucking root lmao
    - OK. **I can create an Ansible playbook to create the users, and then use that Ansible playbook in the Dockerfile** !! There we go
- Docker has limited plugin support, but one of the things it can support is volume plugins - could that work?
    - <http://docs.docker.com/engine/extend/plugins/>
    - One of them is a Keywhiz plugin, which looks like an interesting way to handle storing of secrets

Also, I should separate the Docker parts and the Ansible parts. I honestly probably shouldn't be using Ansible to *build* the Docker images - I should just do that on my workstation by invoking Docker directly, and I should upload it to my Docker registry.

- This is not how I've build my roles so far: 
    - apache
    - discourse-sever
    - ghost-server
    - openvpn-server
- But it IS how I want to build all my roles going forward. 
- I should fix my old roles when I get around to it
    - ...but idk if I'll even use them at this point. This is an aside but I'm considering:
        - Using a constellation of tumblr blogs as my blog
        - Using a GitHub pages site as my landing page + static content
        - This wouldn't let me do the comments thing I want, where my friends can log in with any social media they have (Facebook, Twitter, anything), but it would let me get started blogging right away
        - I can always try the comment thing again later with either Wordpress or Ghost
        - I could use the recent Wordpress Calypso thing... which I could also host on GH Pages if I wanted b/c it's a single page app




