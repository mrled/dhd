#!/bin/csh
# First configures and builds Carbon Emacs, then X11 emacs. Installs X11 emacs
# as /usr/local/bin/emacs (can be used from either cli or X). 
# If you don't want Carbon or X11 emacs, note: `make bootstrap` does a regular
# make as well, so  unless you run configure twice, you don't need to do both.

setenv DATE `/bin/date +%Y%m%d`
setenv SRCPATH "~/opt/src/emacs/"
setenv BLDPATH "~/opt/src/emacs/Build-$DATE/"

cd $SRCPATH/
cvs up

mkdir $BLDPATH ; cd $BLDPATH/

$SRCPATH/configure --enable-carbon-app \
    --with-xpm --with-jpeg --with-tiff --with-gif --with-png && \
make bootstrap && \
$SRCPATH/configure --with-x && \
make



