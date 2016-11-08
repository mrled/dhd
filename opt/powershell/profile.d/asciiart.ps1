. $PSScriptRoot/os.ps1

function AsciiWriteHostWrapper {
    param(
        [parameter(Position=0)] [string] $message = "",
        [Alias('f')] $foregroundColor,
        [Alias('b')] $backgroundColor
    )
    $pp = @{}
    $pp['NoNewline'] = $true
    $pp['Object'] = $message
    if ($foregroundColor) { $pp['foregroundColor'] = $foregroundColor } 
    if ($backgroundColor) { $pp['backgroundColor'] = $backgroundColor } 

    write-host @pp
}
set-alias whw AsciiWriteHostWrapper
function Newline {
    write-host ""
}

<#
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
#>

function Show-Hatchet {
    newline
    whw "  /'-./\_" -f white;  newline
    whw " :    "    -f white;  whw -f darkred "||";           whw ",>" -f white;  newline
    whw "  \.-'"    -f white;  whw -f darkred "||";                               newline
    whw "      ||" -f darkred; whw -f green "   BURIED";                          newline
    whw "      ||" -f darkred; whw -f green "       HATCHET";                     newline
    whw "      ||" -f darkred; newline
    newline
}

<#
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
#>

function Show-Metroid {
    $EE = "$([char]926)$([char]926)"
    whw -f green "  .---."; newline
    whw -f green " /  ";      whw -f red   "@" ;    whw -f green  "  \";  newline
    whw -f green " \ ";       whw -f red  "@ @";    whw -f green   " /";  newline
    whw -f yellow "  {'";     whw -f green "^";     whw -f yellow "'}";   newline
    whw -f white "     -- SQU$EE!!"; newline
}

function Show-BWWindowsLogo {
    # Originally from http://www.asciiworld.com/-Logos,50-.html
    write-host '            ,-~¨^  ^¨-,           _,'
    write-host '           /          / ;^-._...,¨/'
    write-host '          /          / /         /'
    write-host '         /          / /         /'
    write-host '        /          / /         /'
    write-host "       /,.-:''-,_ / /         /"
    write-host '       _,.-:--._ ^ ^:-._ __../'
    write-host '     /^         / /¨:.._¨__.;'
    write-host '    /          / /      ^  /'
    write-host '   /          / /         /'
    write-host '  /          / /         /'
    write-host ' /_,.--:^-._/ /         /'
    write-host '^            ^¨¨-.___.:^'
}

function Show-CurvyWindowsLogo {
    write-host ''
    write-host -nonewline -foreground red  '       ,-~ ~-,' ; write-host -foreground green       ' ,_~ ~-,'
    write-host -nonewline -foreground red  '      /     /'  ; write-host -foreground green      ' /     / '
    write-host -nonewline -foreground red  '     /,- -,/'   ; write-host -foreground green     ' /,- -,/  ' -nonewline; write-host "  Powershell $powershellVersion"
    write-host -nonewline -foreground blue '    ,-~ ~-,'    ; write-host -foreground yellow   ' ,_~ ~-,   ' 
    write-host -nonewline -foreground blue '   /     /'     ; write-host -foreground yellow  ' /     /    ' -nonewline; write-host "  $osVersion"
    write-host -nonewline -foreground blue '  /,- -,/'      ; write-host -foreground yellow ' /,- -,/     '
    write-host ''
}

<#
function Show-SquareWindowsLogo {
    $spaces = '     '
    $pv = "  Powershell $powershellVersion"
    $wv = "  $osVersion"
    write-host -nonewline ' '; write-host -nonewline -background red  $spaces; write-host -background green  $spaces
    write-host -nonewline ' '; write-host -nonewline -background red  $spaces; write-host -background green  $spaces -nonewline; write-host $pv
    write-host -nonewline ' '; write-host -nonewline -background blue $spaces; write-host -background yellow $spaces -nonewline; write-host $wv
    write-host -nonewline ' '; write-host -nonewline -background blue $spaces; write-host -background yellow $spaces
}
#>

function Show-SquareWindowsLogo {
    $spaces = '     '
    $pv = "  Powershell $powershellVersion"
    $wv = "  $osVersion"
    whw ' '; whw -b red  $spaces; whw -b green  $spaces ;             newline
    whw ' '; whw -b red  $spaces; whw -b green  $spaces ; whw "$pv" ; newline
    whw ' '; whw -b blue $spaces; whw -b yellow $spaces ; whw "$wv" ; newline
    whw ' '; whw -b blue $spaces; whw -b yellow $spaces ;             newline
}

