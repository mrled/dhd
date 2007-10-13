#!/bin/csh

# Update fink itself, as well as its package descriptions.
sudo fink selfupdate
sudo /sw/lib/fink/postintstall.pl
sudo apt-get update
sudo fink selfupdate

# Update the packages.
sudo fink update-all
