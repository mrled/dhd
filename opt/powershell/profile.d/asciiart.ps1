function Display-Hatchet {
    write-host ""
    write-host "  /'-./\_"  -foreground white
    write-host " :    " -foreground white -nonewline; write-host "||" -foreground darkred -nonewline; write-host ",>" -foreground white
    write-host "  \.-'" -foreground white -nonewline; write-host "||" -foreground darkred
    write-host "      ||" -foreground darkred -nonewline; write-host "   BURIED" -foreground green
    write-host "      ||" -foreground darkred -nonewline; write-host "       HATCHET" -foreground green
    write-host "      ||" -foreground darkred
    write-host ""
}

function Display-Metroid {
    #  .---.
    # /  @  \
    # \ @ @ /
    #  {'^'}
    #     -- SQUEE!!
    write-host "  .---."  -foreground green
    write-host " /  " -foreground green -nonewline; write-host "@" -foreground red -nonewline; write-host "  \" -foreground green
    write-host " \ " -foreground green -nonewline; write-host "@ @" -foreground red -nonewline; write-host " /" -foreground green
    write-host "  {'" -foreground yellow -nonewline; write-host "^" -foreground green -nonewline; write-host "'}" -foreground yellow
    write-host "     -- SQUEE!!" -foreground white
}