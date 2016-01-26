#!/usr/bin/env python3

import datetime
import os

scriptdir = os.path.dirname(os.path.realpath(__file__))
logpath = os.path.join(scriptdir, 'log.txt')

def log(text):
    datestring = datetime.datetime.now().isoformat()
    datedtext = "{} -- {}".format(datestring, text)
    with open(logpath, 'a') as logfile:
        print(datedtext, file=logfile)

def application(environ, start_response):
    status = '200 OK'
    output = b'Hello World!\n'
    response_headers = [
        ('Content-type', 'text/plain'),
        ('Content-Length', str(len(output)))]
    start_response(status, response_headers)
    log("Sup from a script at '{}'".format(__file__))
    return [output]
