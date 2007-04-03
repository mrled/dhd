#!/bin/csh

cd ~/opt/src/emacs/cvs
./configure \
    --enable-carbon-app=/Applications \
    --with-xpm --with-jpeg --with-tiff --with-gif --with-png
make bootstrap #does a regular `make` as well
sudo make install
