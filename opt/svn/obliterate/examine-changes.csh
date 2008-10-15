#!/bin/csh

# This looks at the log done by restore.csh and looks at all the files that ARE in your repository AFTER you have made the changes. If something is in that list that shouldn't be, then you should re-run the remove.csh command. 
grep \* load.VICTORY.log | cut -d' ' -f10 | sort | uniq | less
