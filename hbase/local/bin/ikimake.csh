#!/bin/tcsh

umask 022
/usr/local/bin/ikiwiki --verbose ~/doc/remote/wikiwc ~/Sites/Documents/wiki --url=http://AndrAIa.local/wiki

if "$1" == "push" 
    git commit -m '' 
    git push 
