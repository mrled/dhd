#!/bin/bash

# Create SSBs based on Chromium that do not share cookies or other data
#
# This is meant to be used with something like stumpwm (or at least a cli alias)
# Where you can just be like FACEBOOK and bam, facebook
# Maybe I should rename this to bam. 
# 
# To do: 
# - Cookie whitelisting, defaulting to the domain you provide on the command line
# - Handle URLs with paths in them better (i.e. at all)
# - Handle URLs with the protocol in them better (i.e. at all)
# - Make this work with stumpwm's run-or-raise thingy
# - Maybe allow renaming of things for even lazier access, like facebook.com -> fb
# - After all that, just copy Fluid.app and make it even better, but for Windows and Linux. Should be easy, right? 
# 
# Notes:
# - I first created a chromium user profile
# - I turned off plugins and made some other small adjustments
# - That is now in $base_config (below). I guess you'll need to do this on each host / user (for now). 
# - It gets copied to $udd_base/sub.domain.tld if that folder doesn't already exist
# - That's really it. Pretty dumb, but it works and maybe increases privacy a litte. 

DEBUG=true #must be set to 'true' or 'false' 

debug() {
    if $DEBUG; then echo "DEBUG: $@"; fi
}

cmdname=$(basename $0)

help() {
    echo "$cmdname runs Chromium site-specific browsers that do not share cookies or other data." 
    echo "$cmdname [sub.]domain.tld"
    echo "Do NOT use full URLs with the protocol (http://) or a slash after wards (facebook.com/) or a file or directory name afterwards (/index.html)."
}

# detect if there are any slashes in $1
if echo $1 | grep / ; then
    help
    exit
fi

udd_base="${HOME}/.chromium-ssb"
base_config="$udd_base/datadir.base"
url="https://$1" #if the site is not https aware then durrr
udd="$udd_base/$1" #user data dir

debug url: $url
debug udd: $udd

if [[ ! -d "$udd" ]]; then
    debug 'udd does not exist; creating with this command: '
    debug cp -r "$base_config" "$udd"
    cp -r "$base_config" "$udd"
fi

debug chromium-browser --user-data-dir="$udd" --app="$url"
chromium-browser --user-data-dir="$udd" --app="$url"