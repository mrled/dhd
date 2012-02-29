#!/bin/bash

# Setting this is THE ONLY WAY to make dates show up yyyy-mm-dd, isn't that a load of bullshit?
export LC_TIME=en_DK.utf8

/usr/bin/thunderbird -P Neuric -no-remote &
/usr/bin/thunderbird -P Home -no-remote &
