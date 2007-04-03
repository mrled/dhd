#!/bin/csh
setenv DATE `/bin/date +%Y%m%d`
setenv BDIR "~/Backup/$DATE"
mkdir -p $BDIR
cp -r ~/Library/Application\ Support/Camino $BDIR
