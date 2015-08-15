# Purge docker

Docker keeps around images you've downloaded forever. 

<http://jimhoskins.com/2013/07/27/remove-untagged-docker-images.html>

That helps, but its syntax isn't quite right

I need to find a way to do the following programatically

- delete _tmp images
- delete untagged images
- delete old containers (e.g. shit that isn't running, but that you can see from docker ps -a)
- list images and sanity check that they there aren't too many
