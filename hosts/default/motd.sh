#!/bin/sh
# Shell MOTD
# Expects DHD/shdetect in scope

if dhd_cmdavail lolcatjs; then
    alias outfilter=lolcatjs
else
    alias outfilter=cat
fi

if dhd_cmdavail figlet; then
    if test -d "$DHD_FIGLET_FONTS"; then
        echo ""
        banner="$(whoami) @ $(hostname -s)"
        figlet -f "$DHD_FIGLET_FONTS/future" "$banner" |
            outfilter
    fi
fi

if dhd_cmdavail fortune; then
    fortdb="$FORTUNATE_CHECKOUT/invisiblestates/invisiblestates"
    if test -d "$FORTUNATE_CHECKOUT"; then
        fortune "$fortdb" |
            fold -w 80 -s |
            outfilter
    fi
fi
