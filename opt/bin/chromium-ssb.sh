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
protocol=unspecified

usage() {
    echo "USAGE: $cmdname runs Chromium site-specific browsers that do not share cookies or other data." 
    echo "USAGE: $cmdname [sub.]domain.tld"
}


if [[ $1 == "" ]]; then
    echo "You didn't specify a URL." 
    usage
    exit
fi

udd_base="${HOME}/.chromium-ssb"
base_config="$udd_base/datadir.base"


#### MUNGE URL: 
# slashes aren't allowed, but let's massage the input to see if we can cope
# NOTES to usage you follow along: 
# - $input is just the raw value of $1, which we expect to be a URL like http://facebook.com/
# - $input2 is the above, stripped of its trailing slash (if it has one)
# - $input3 is the above, also stripped of its protocol specifier (if it has one)
# - if $input3 still has slashes in it, then you've passed a path (like facebook.com/index.html), which we don't support
input=$1
input2=`echo $input | sed 's/\(.*\)\/$/\1/'`

if echo $input2 | grep '://'; then
    # the sed statement yanks out anything before a ://
    protocol=`echo $input2 | sed 's/^\([a-zA-Z0-9]*\):\/\/\(.*\)/\1/'`
    if [[ $protocol -ne "http" ]] && [[ $protocol -ne "https" ]] ; then
        echo "Sorry, we don't support the $protocol protocol." 
        usage
        exit
    fi
    # similar to the previous statement, but yanks out anything AFTER a ://
    input3=`echo $input2 | sed 's/^\([a-zA-Z0-9]*\):\/\/\(.*)/\2/'`
else
    # assume the HTTPS protocol. This is probably OK? 
    protocol="https"
    input3=$input2
fi

if echo $input3 | grep '/'; then
    echo "Your URL contains a path specifier (such as facebook.com/index.html). Please only pass URLs in the base of the domain (like facebook.com)." 
    usage
    exit
fi

url="$protocol://$input3"
userdata="$udd_base/$input3"
debug url: $url
debug userdata: $userdata

if [[ ! -d "$userdata" ]]; then
    debug 'userdata does not exist; creating with this command: '
    debug cp -r "$base_config" "$userdata"
    cp -r "$base_config" "$userdata"
fi

debug chromium-browser --user-data-dir="$userdata" --app="$url"
chromium-browser --user-data-dir="$userdata" --app="$url"