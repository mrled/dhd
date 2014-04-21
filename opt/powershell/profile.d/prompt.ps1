﻿function displayPath {
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
        $displaypath = $splitpath[0] # drive letter or ~
        $displaypath+= '\...\'
        $displaypath+= $splitpath[$splitpath.count-1] #the last folder in the path
    }
    else {
        $displaypath = $path
    }
    return $displaypath
}

# This lets you define a $promptSuffix scriptblock variable elsewhere.
# I use this for my DLP SolutionScripts.profile.ps1 for example. 
if (-not $promptSuffix) {
    if ($SoyAdmin) {
        #Write-Host ("PS#") -nonewline -foregroundcolor White -backgroundcolor Red
        # [char]9773   ☭ (HAMMER AND SICKLE)
        $promptSuffix = { write-host " $([char]9773) " -nonewline -foregroundcolor red -backgroundcolor yellow }
    }
    else {
        #$promptSuffix = { Write-Host "PS>" -nonewline -foregroundcolor White } 
        #$promptSuffix = { Write-Host "♫" -nonewline -foreground White  } 
        #$promptSuffix = { Write-Host "PS»" -nonewline -foreground White }
        $promptSuffix = { Write-Host "PS$([char]187)" -nonewline -foreground White }
    }
}

# A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
# doesn't play nice with other things. 
function colorPrompt {
    #$Host.UI.RawUI.WindowTitle = $pwd
    Write-Host $(get-date -format HH:mm:ss) -nonewline -foregroundcolor White
    Write-Host " $hostname" -nonewline -foregroundcolor Blue
    Write-Host " $(displayPath $pwd) " -nonewline -foregroundcolor Green
    
    invoke-command $promptSuffix

    # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
    return " "
}

# A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
function simplePrompt {
    if ($SoyAdmin) { $lcop = "#" }
    else { $lcop = ">" }
    return "$(get-date).Tostring('HH:mm:ss') $hostname $(displayPath $pwd) PS$lcop "
}

if ($env:term -eq "emacs") {
    # Emacs' "M-x powershell" seems to handle the prompt itself, and you get extra newlines if you 
    # define one 
    if (test-path function:\prompt) { del function:\prompt }
}
else {
    function global:prompt { colorPrompt }
}

