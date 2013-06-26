#!/usr/bin/python3 
# -*- mode: python -*-

""" Query RT

Config file example: 

    [general]
    username = <username>
    password = <rt password>
    
    [rtserver]
    cacert = ~/path/to/cacert.pem
    host = rtserver.domain.tld
    
    [svn.<repo name>]
    url = https://svnserver.domain.tld/path/to/repo
    viewvc = https://svnserver.domain.tld/path/to/repo #this feature relies on viewvc being installed
    desc = <repo description>
    exclude = /Path/To/Exclude # path (relative to SVN server) to exclude commits from
    
Note that the svn.whatever sections are used only by rmet2, and that you can have as many of them as you want.

"""

# Query RT

# http://requesttracker.wikia.com/wiki/REST

import sys
import os
import time
#import subprocess
import re
import datetime
#import xml.etree.ElementTree as ET
import http.client
import ssl
import argparse
import urllib
import configparser

configfile=os.path.abspath(os.path.expanduser('~/.qrt_config'))

QRT_DEBUG=False
def debugprint(text):
    if QRT_DEBUG:
        print("DEBUG: " + str(text))

class Ticket:
    def __init__(self, name="", tid=None, rtserver=None):
        if not tid:
            raise Exception("Tried to create a ticket with no ticket id; this is useless.")
        self.tid=tid

        if not rtserver:
            raise Exception("Tried to create a ticket with no rtserver; this is useless.")
        self.rtserver=rtserver

        self.url = "{}/Ticket/Display.html?id={}".format(self.rtserver.baseurl, self.tid)
        self.name=name

    def markdownify(self, nobullet=False):
        if nobullet:
            # return just the raw link
            return "[{}]({})".format(self.name, self.url)
        else:
            # return a bulleted link
            return "- [{}]({})".format(self.name, self.url)

    def __repr__(self):
        return "{}: {}".format(self.tid, self.name)

class RTServer:
    def __init__(self, rtserver, ssl=False, cacert=False, username="", password=""):
        self.rtserver = rtserver
        self.ssl=ssl
        self.cacert=cacert
        self.username=username
        self.password=password
        self.bodyauth = "user={}&pass={}".format(username, password)
        self.setup_connection()

        self.baseurl= "https://" if ssl else "http://"
        self.baseurl += rtserver

    def setup_connection(self):
        if self.ssl:
            context = ssl.SSLContext(ssl.PROTOCOL_TLSv1)
            context.verify_mode = ssl.CERT_REQUIRED
            context.load_verify_locations(self.cacert)
            self.conn = http.client.HTTPSConnection(self.rtserver, context=context)
        else:
            self.conn = http.client.HTTPConnection(self.rtserver)

    def get_ticket(self, tid):
        urlpath=urllib.parse.quote("/REST/1.0/ticket/{}/show".format(str(tid)).format("utf8"))
        qurlpath=urllib.parse.quote(urlpath.encode("utf8"))
        debugprint(qurlpath)
        #self.conn = self.setup_connection()
        self.conn.request("POST", qurlpath, body=self.bodyauth)
        response = self.conn.getresponse()
        return response.read().decode()

    def encode_query(self, query):
        """ Encode a query string.
        RT seems to require that some things in a query string (like spaces) to be URL encoded,
        but other things (like "'<>=) not to be. Whatever. Don't use urllib.parse.quote() then.
        """
        equery = ""
        for character in query:
            if character == " ":
                equery += "%20"
            else:
                equery += character
        return equery

    def get_query_results(self, query):
        urlpath="/REST/1.0/search/ticket?query={}".format(query).format("utf8")
        #qurlpath=urllib.parse.quote(urlpath.encode("utf8"))
        qurlpath = self.encode_query(urlpath)
        debugprint(qurlpath)
        self.conn.request("POST", qurlpath, body=self.bodyauth)
        response = self.conn.getresponse()
        #return response.read().decode()

        response_array = response.read().decode().splitlines()[2:]
        if response_array[0] == 'No matching results.':
            debugprint("RT has no matching results, returning false...")
            return False
        ticket_array=[]
        for ticketline in response_array:
            m = re.match("([0-9]*): (.*)", ticketline)
            tid, tname = int(m.group(1)), m.group(2)
            ticket = Ticket(tid=tid, name=tname, rtserver=self)
            ticket_array.append(ticket)
        
        return ticket_array

def main(*args):
    argparser = argparse.ArgumentParser()

    h="Query for the RT API. Can be an integer, which is interpreted as a request to display a "
    h+="ticket ID, or a string, which is interpreted as a query to pass to RT's search."
    argparser.add_argument('query', action='store', help=h)
    args = argparser.parse_args()

    config = configparser.ConfigParser()
    config.read(configfile)

    RT = RTServer(config['rtserver']['host'],
                  ssl=True if config['rtserver']['cacert'] else False,
                  cacert=os.path.abspath(os.path.expanduser(config['rtserver']['cacert'])),
                  username = config['general']['username'],
                  password = config['general']['password'])

    try:
        # attempt to see the query as an int
        print(RT.get_ticket(int(args.query)))
    except ValueError:
        # didn't work, use it as a string
        results = RT.get_query_results(args.query)
        for r in results:
            print(r)

if __name__ == '__main__':
    sys.exit(main(*sys.argv))
