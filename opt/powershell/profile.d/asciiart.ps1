function Show-Hatchet {
    write-host ""
    write-host "  /'-./\_"  -foreground white
    write-host " :    " -foreground white -nonewline; write-host "||" -foreground darkred -nonewline; write-host ",>" -foreground white
    write-host "  \.-'" -foreground white -nonewline; write-host "||" -foreground darkred
    write-host "      ||" -foreground darkred -nonewline; write-host "   BURIED" -foreground green
    write-host "      ||" -foreground darkred -nonewline; write-host "       HATCHET" -foreground green
    write-host "      ||" -foreground darkred
    write-host ""
}

function Show-Metroid {
    #  .---.
    # /  @  \
    # \ @ @ /
    #  {'^'}
    #     -- SQUEE!!
    $EE = "$([char]926)$([char]926)"
    write-host "  .---."  -foreground green
    write-host " /  " -foreground green -nonewline; write-host "@" -foreground red -nonewline; write-host "  \" -foreground green
    write-host " \ " -foreground green -nonewline; write-host "@ @" -foreground red -nonewline; write-host " /" -foreground green
    write-host "  {'" -foreground yellow -nonewline; write-host "^" -foreground green -nonewline; write-host "'}" -foreground yellow
    #write-host "     -- SQUΞΞ!!" -foreground white
    write-host "     -- SQU$EE!!" -foreground white
}

