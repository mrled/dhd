#!/bin/csh

cd ~/opt/src/emacs/

# do Carbon Emacs
./configure \
    --enable-carbon-app \
    --with-xpm --with-jpeg --with-tiff --with-gif --with-png
make bootstrap #does a regular `make` as well

# do X11 Emacs; makes /usr/local/bin/emacs be X11 emacs, not just cli emacs
./configure --with-x
make

# Install them both
#sudo make install
