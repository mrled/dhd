#!/usr/bin/env python
import sys, os, re, urllib, json
from BeautifulSoup import BeautifulSoup

def isblipurl(url):
    if re.match(r'http://blip.tv',url):
        return 1
    elif re.match(r'blip.tv',url):
        return 1
    else:
        return 0

def issoundcloudurl(url):
    if re.match(r'http://soundcloud.com',url):
        return 1
    elif re.match(r'soundcloud.com',url):
        return 1
    else:
        return 0

def getsoundcloudurl(scurl):
    urlcontents = urllib.urlopen(scurl).read()
    wholepagesoup = BeautifulSoup(urlcontents)
    maincontentinner = wholepagesoup.find(id="main-content-inner") #find this div ID so you're not also grabbing media that's in the sidebar
    # pages for one track will have just one thing in this array, but pages such as band pages might have more than one:
    for bufferjs in maincontentinner.findAll(text=re.compile(r"window.SC.bufferTracks.push")): 
        bufferjs = bufferjs.replace("\nwindow.SC.bufferTracks.push(","")
        bufferjs = bufferjs.replace(");\n","")
        bufferjson = json.loads(bufferjs)
        streamurl = bufferjson["streamUrl"]
        filename = bufferjson["user"]["username"] + " - " + bufferjson["title"] + " (soundcloud rip).mp3"
        print "Downloading file: " + filename
        urllib.urlretrieve(streamurl,filename)

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

def processentry(arg,filename=""):
    if   isblipurl(arg):
        getblipurl(arg)
    elif issoundcloudurl(arg):
        getsoundcloudurl(arg)
    elif not filename and os.path.isfile(arg): # check to see if we're in a file; don't allow files to point to other files, there's just no reason for this
        inputfilename = arg
        inputfile = open(arg,'r')
        for line in inputfile:
            processentry(line,inputfilename) #pass the input filename so we can use it in error messages (is that actually necessary?)
        inputfile.close
        in_file = 0
    else:
        output = ""
        if in_file:
            output = "In file: " + filename + ": "
        output += "Ignoring entry " + arg + " because it is neither a local file nor a valid url that I can parse"
        print output

for arg in sys.argv[1:]:
    processentry(arg)

