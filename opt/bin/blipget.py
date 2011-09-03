#!/usr/bin/env python
import json
import urllib
import sys
import re

for blipurl in sys.argv[1:]:
    blipurl += "?skin=json"
    urlcontents=urllib.urlopen(blipurl).read()
    # the json is wrapped in some sort of javascript construct thing - a function definition or something? 
    # (as you can tell I'm very knowledgeable about javascript.)
    urlcontents=urlcontents.replace("blip_ws_results([","")
    urlcontents=urlcontents.replace("]);","")
    # print urlcontents
    #
    blipjson=json.loads(urlcontents)
    mediaurl=blipjson["Post"]["media"]["url"]
    origurl=blipjson["Post"]["url"])
    mediafilename=re.sub('.*/','',origurl) + ".mp4"
    #
    print "Downloading file: " mediafilename
    urllib.urlretrieve(mediaurl,mediafilename)
