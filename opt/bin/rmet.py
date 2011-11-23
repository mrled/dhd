#!/usr/bin/python3 

import sys

def mungeoutput(output):
    headerfooter = re.compile("(id\tSubject\n)|(\n$)") #the header is id\tSubject\n and the footer is just an extra \n
    linebegin = re.compile("^|\n") #the begining of the line OR of the whole block of text
    eof = re.compile("$")
    ticketline = re.compile("([0-9]*)\t(.*)")
    
    output = str(output, "utf-8") #required b/c it comes to us from subprocess.check_output which returns a bytes object
    output = re.sub(headerfooter, r"", output)
    output = re.sub(linebegin, r"\n- ", output)
    output = re.sub(eof, r"\n", output)

    output = re.sub(ticketline, r"[\2](https://bugs.neuric.internal/Ticket/Display.html?id=\1)", output)

    return output

import os, time, subprocess, re

rtbin="/opt/rt40/bin/rt"

today=time.strftime("%Y%m%d")

if (len(sys.argv) <= 1):
    numdays=6
else:
    numdays=sys.argv[1]

updated="Updated >= '" + str(numdays) + " days ago'"

resolvedtickets=mungeoutput(subprocess.check_output([rtbin, "ls", "-f", "Subject", "-o", "-Created", 
                                                     "Queue = '5' AND Status = 'resolved' AND " + updated]))
modifiedtickets=mungeoutput(subprocess.check_output([rtbin, "ls", "-f", "Subject", "-o", "-Created", 
                                                     "Queue = '5' AND Status != 'resolved' AND Status != 'rejected' AND " + updated]))
rejectedtickets=mungeoutput(subprocess.check_output([rtbin, "ls", "-f", "Subject", "-o", "-Created", 
                                                     "Queue = '5' AND Status = 'rejected' AND " + updated]))

print("# Micah's progress report as of " + today + "\n")
print("Tickets I resolved this week:")
print(resolvedtickets)
print("Other tickets I worked on this week:")
print(modifiedtickets)
print("Tickets I rejected this week:")
print(rejectedtickets)
