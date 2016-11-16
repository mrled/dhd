## Internal stuff
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
function Newline {write-host ""}
$psv = "Powershell $($PsVersionTable.PSVersion.ToString())"
try {
    $osv = [System.Environment]::OSVersion.VersionString
}
catch {
    $osv = /usr/bin/uname -a
}

## Exported functions

function Show-AsciiHatchet {
    newline
    whw "  /'-./\_" -f white;  newline
    whw " :    "    -f white;  whw -f darkred "||";           whw ",>" -f white;  newline
    whw "  \.-'"    -f white;  whw -f darkred "||";                               newline
    whw "      ||" -f darkred; whw -f green "   BURIED";                          newline
    whw "      ||" -f darkred; whw -f green "       HATCHET";                     newline
    whw "      ||" -f darkred; newline
    newline
}

function Show-AsciiMetroid {
    $EE = "$([char]926)$([char]926)"
    whw -f green "  .---."; newline
    whw -f green " /  ";      whw -f red   "@" ;    whw -f green  "  \";  newline
    whw -f green " \ ";       whw -f red  "@ @";    whw -f green   " /";  newline
    whw -f yellow "  {'";     whw -f green "^";     whw -f yellow "'}";   newline
    whw -f white "     -- SQU$EE!!"; newline
}

function Show-AsciiBWWindowsLogo {
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

function Show-AsciiCurvyWindowsLogo {
    newline
    whw -f red  '       ,-~ ~-,' ; whw -f green       ' ,_~ ~-,  ' ; newline
    whw -f red  '      /     /'  ; whw -f green      ' /     /   ' ; newline
    whw -f red  '     /,- -,/'   ; whw -f green     ' /,- -,/    ' ; whw $psv ; newline
    whw -f blue '    ,-~ ~-,'    ; whw -f yellow   ' ,_~ ~-,     ' ; newline
    whw -f blue '   /     /'     ; whw -f yellow  ' /     /      ' ; whw $osv ; newline
    whw -f blue '  /,- -,/'      ; whw -f yellow ' /,- -,/       ' ; newline
    newline
}

function Show-AsciiSquareWindowsLogo {
    $spaces = '     '
    whw ' '; whw -b red  $spaces; whw -b green  $spaces ;              newline
    whw ' '; whw -b red  $spaces; whw -b green  $spaces ; whw "$psv" ; newline
    whw ' '; whw -b blue $spaces; whw -b yellow $spaces ; whw "$osv" ; newline
    whw ' '; whw -b blue $spaces; whw -b yellow $spaces ;              newline
}

Export-ModuleMember -function Show-Ascii*
