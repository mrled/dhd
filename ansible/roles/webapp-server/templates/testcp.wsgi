#!/usr/bin/env python3

import sys
sys.stdout = sys.stderr

import atexit
import threading
import cherrypy

cherrypy.config.update({'environment': 'embedded'})

class Root(object):
    def index(self):
        return 'Hello World!\n'
    index.exposed = True

application = cherrypy.Application(Root(), script_name=None, config=None)
