#!/bin/tcsh

setenv HOST "mrled.org"
setenv USER "mrled"
setenv DIR "/home/$USER"
setenv UHOST "$USER@$HOST"
sshfs $UHOST:$DIR /.V/$UHOST -oreconnect,ping_diskarb,volname="$UHOST"

setenv HOST "mrled.org"
setenv USER "younix"
setenv DIR "/home/$USER"
setenv UHOST "$USER@$HOST"
sshfs $UHOST:$DIR /.V/$UHOST -oreconnect,ping_diskarb,volname="$UHOST"

setenv HOST "vlack.com"
setenv USER "mledbetter"
setenv DIR "/home/$USER"
setenv UHOST "$USER@$HOST"
sshfs $UHOST:$DIR /.V/$UHOST -oreconnect,ping_diskarb,volname="$UHOST"

setenv HOST "vlack.com"
setenv USER "vlack"
setenv DIR "/home/$USER"
setenv UHOST "$USER@$HOST"
sshfs $UHOST:$DIR /.V/$UHOST -oreconnect,ping_diskarb,volname="$UHOST"

setenv HOST "green-slime.utexas.edu"
setenv USER "mrled"
setenv DIR "/home/$USER"
setenv UHOST "$USER@$HOST"
sshfs $UHOST:$DIR /.V/$UHOST -oreconnect,ping_diskarb,volname="$UHOST"


