#!/usr/bin/env python
import json
import urllib
import sys
import re
import os

def isblipurl(url):
    if re.match(r'http://blip.tv',url):
        return 1
    elif re.match(r'blip.tv',url):
        return 1
    else:
        return 0

def getblipurl(blipurl): 
    blipurl += "?skin=json"
    urlcontents=urllib.urlopen(blipurl).read()
    # the json is wrapped in some sort of javascript construct thing - a function definition or something? 
    # (as you can tell I'm very knowledgeable about javascript.)
    urlcontents=urlcontents.replace("blip_ws_results([","")
    urlcontents=urlcontents.replace("]);","")
    # 
    blipjson=json.loads(urlcontents)
    mediaurl=blipjson["Post"]["media"]["url"]
    origurl=blipjson["Post"]["url"]
    mediafilename=re.sub('.*/','',origurl) + ".mp4"
    #
    print "Downloading file: " + mediafilename
    urllib.urlretrieve(mediaurl,mediafilename)


for arg in sys.argv[1:]:
    if isblipurl(arg):
        getblipurl(arg)
    elif os.path.isfile(arg):
        argfile = open(arg,'r')
        for line in argfile:
            if isblipurl(line):
                getblipurl(line)
            else:
                print argfile + ": Skipping line: " + line + ": Not a valid blip url in the form (http://)?blip.tv.*"
        file.close
    else:
        print "Ignoring argument " + arg + " because it is neither a local file nor a valid blip url in the form (http://)?blip.tv.*"
    exit

