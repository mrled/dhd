#!/bin/sh

if dhd_cmdavail rainbow; then
    alias outfilter=rainbow
else
    alias outfilter=cat
fi

if dhd_cmdavail figlet; then
    if test -d "$DHD_FIGLET_FONTS"; then
        banner="usb ports like cocaine"
        figlet -f "$DHD_FIGLET_FONTS/future" "$banner" |
            outfilter
    fi
fi

if dhd_cmdavail fortune; then
    if test -d "$FORTUNATE_CHECKOUT"; then
        fortune "$FORTUNATE_CHECKOUT"/tweets/*.tweets |
            fold -w 80 -s |
            outfilter
    fi
fi
