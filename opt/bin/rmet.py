#!/usr/bin/python3 

import sys, os, time, subprocess, re

def mungeoutput(output):
    #match a header of id\tSubject\n and a footer of just an extra \n
    headerfooter = re.compile("(id\tSubject\n)|(\n$)") 

    #match the begining of the line OR of the whole block of text
    linebegin = re.compile("^|\n") 

    #match an eof
    eof = re.compile("$")

    #match a ticket line, consisting of a ticket number, a tab, and a title
    #like "1234	Fix some horrifying problem by yesterday"
    ticketline = re.compile("([0-9]*)\t(.*)")

    #required b/c it comes to us from subprocess.check_output which returns a bytes object
    output = str(output, "utf-8") 

    ##now process the regexps

    #nuke header and footer
    output = re.sub(headerfooter, r"", output)

    #change line beginings to have bullets in front of them
    output = re.sub(linebegin, r"\n- ", output)

    #add an extra line at the end for... some reason
    output = re.sub(eof, r"\n", output)

    #make the ticket line into a Markdown link
    output = re.sub(ticketline, r"[\2](https://bugs.neuric.internal/Ticket/Display.html?id=\1)", output)

    return output


rtbin="/opt/rt40/bin/rt"
today=time.strftime("%Y%m%d")

# By default, check the last 6 days, but use the first argument if present
if (len(sys.argv) <= 1):
    numdays=6
else:
    numdays=sys.argv[1]
updated="Updated >= '" + str(numdays) + " days ago'"


# Find resolved tickets
cmd=[rtbin, "ls", "-f", "Subject", "-o", "-Created", 
     "Queue = '5' AND Status = 'resolved' AND " + updated]
resolvedtickets=mungeoutput(subprocess.check_output(cmd))

# Find modified tickets
cmd=[rtbin, "ls", "-f", "Subject", "-o", "-Created", 
     "Queue = '5' AND Status != 'resolved' AND Status != 'rejected' AND " + updated]
modifiedtickets=mungeoutput(subprocess.check_output(cmd))

# Find rejected tickets
cmd=[rtbin, "ls", "-f", "Subject", "-o", "-Created",
     "Queue = '5' AND Status = 'rejected' AND " + updated]
rejectedtickets=mungeoutput(subprocess.check_output(cmd))

# Now print them all. I'm using Markdown, which means it's easy to just do `rmet.py|markdown.pl`
# or if I want to add notes or something I can do it in markdown format at the bottom first
print("# Micah's progress report as of " + today + "\n")
print("Tickets I resolved this week:")
print(resolvedtickets)
print("Other tickets I worked on this week:")
print(modifiedtickets)
print("Tickets I rejected this week:")
print(rejectedtickets)

