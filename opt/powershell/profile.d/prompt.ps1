function displayPath {
    param(
        $path
    )
    switch ($path.gettype()) {
        'PathInfo' {
            $path = $path.providerpath
        }
        'DirectoryInfo' {
            $path = $path.fullname
        }
    }
    $path = $path -replace [regex]::Escape($home),"~"
    $splitpath = $path -split '\\'
    if ($splitpath.count -gt 2) {
        $displaypath = $splitpath[0] # drive letter
        $displaypath+= '\...\'
        $displaypath+= $splitpath[$splitpath.count-1] #the last folder in the path
    }
    else {
        $displaypath = $path
    }
    return $displaypath
}

# A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
# doesn't play nice with other things. 
function colorPrompt {
    try { $realLASTEXITCODE = $LASTEXITCODE }
    catch [System.Management.Automation.RuntimeException] { $realLASTEXITCODE=$null} 

    Write-Host $(get-date).Tostring("HH:mm:ss") -nonewline -foregroundcolor White
    Write-Host (" ") -nonewline
    Write-Host ($hostname) -nonewline -foregroundcolor Blue
    
    $mypwd = displayPath $pwd
    Write-Host (" " + $mypwd + " ") -nonewline -foregroundcolor Green
    
    if ($SoyAdmin) {
        Write-Host ("PS#") -nonewline -foregroundcolor White -backgroundcolor Red
    }
    else {
        Write-Host ("PS>") -nonewline -foregroundcolor White
    }

    $global:LASTEXITCODE = $realLASTEXITCODE

    # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
    return " "
}

# A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
function simplePrompt {
    $dt = $(get-date).Tostring("HH:mm:ss")
    $hn = [System.Net.Dns]::GetHostName()
    
    # if we're on an smb share or something $pwd contains loads of useless bullshit; strip it. 
    # Make some other optimizations for space.
    $mypwd = displayPath $pwd
    
    if ($SoyAdmin) { $lcop = "#" }
    else { $lcop = ">" }
    
    return "$dt $hn $mypwd PS$lcop "
}

if ($env:term -eq "emacs") {
    # Emacs' "M-x powershell" seems to handle the prompt itself, and you get extra newlines if you 
    # define one 
    if (test-path function:\prompt) { del function:\prompt }
}
else {
    function global:prompt { colorPrompt }
}

function Disable-Prompt {
    if (test-path function:\prompt) { del function:\prompt }
}    
function Enable-ColorPrompt { 
    disable-prompt
    function global:prompt { colorPrompt }
}
function Enable-SimplePrompt { 
    disable-prompt
    function global:prompt { simplePrompt }
}
