#!/bin/sh 

mkdir -p ~/opt/share/fortunes
ln -sf ../../../doc/dhd/doc/quote.txt ~/opt/share/fortunes/quote
strfile -rs ~/opt/share/fortunes/quote