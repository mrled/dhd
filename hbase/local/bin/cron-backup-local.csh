#!/bin/csh
setenv DATE `/bin/date %Y%m%d`
mkdir -p ~/Backups/$DATE
cp -r ~/Library/Application\ Support/Camino ~/Backups/$DATE/
