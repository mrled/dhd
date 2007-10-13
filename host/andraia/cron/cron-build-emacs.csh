#!/bin/csh

cd ~/opt/src/emacs/
./configure \
    --enable-carbon-app \
    --with-xpm --with-jpeg --with-tiff --with-gif --with-png
make bootstrap #does a regular `make` as well
#sudo make install
