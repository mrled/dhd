#!/bin/sh

if dhd_cmdavail rainbow; then
    alias outfilter=rainbow
else
    alias outfilter=cat
fi

if dhd_cmdavail figlet; then
    if test -d "$DHD_FIGLET_FONTS"; then
        echo ""
        banner="usb ports like cocaine"
        figlet -f "$DHD_FIGLET_FONTS/future" "$banner" |
            outfilter
    fi
fi

if dhd_cmdavail fortune; then
    fortdb="$FORTUNATE_CHECKOUT/tweets/ctrlcreep.tweets"
    if test -d "$FORTUNATE_CHECKOUT"; then
        fortune "$fortdb" |
            fold -w 80 -s |
            outfilter
    fi
fi
