function Get-DisplayPath {
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

function Get-JobStateColor {
    param(
        [string[]] $status
    )
    $jobStateTable = @{ 
        NeedsAttention = @('Blocked','Disconnected','Failed','Stopped','Stopping','Suspended','Suspending')
        InProgress = @('NotStarted','Running')
        Completed = @('Completed')
    }
    $jobStateTable['All'] = $jobStateTable.Values | % {$_} 

    if ($status.length -eq 0) { 
        return 'White'
    }
    elseif ($status |? { $jobStateTable.All -notcontains $_ }) {
        write-error 'Unknown job status'
        return 'Yellow'
    }
    elseif ($status |? { $jobStateTable.NeedsAttention -contains $_ }) {
        return 'Red'
    }
    elseif ($status |? { $jobStateTable.InProgress -contains $_ }) {
        return 'Cyan'
    }
    elseif ($status |? { $jobStateTable.Completed -contains $_ }) {
        return 'White'
    }
    else {
        write-error 'Unknown job status'
        return 'Yellow'
    }
}

if (-not $PromptSuffixes) { $PromptSuffixes = @{} }
$PromptSuffixes['Admin'] = { write-host " $HammerAndSickleChar " -nonewline -foregroundcolor red -backgroundcolor yellow }
$PromptSuffixes['Default'] = { Write-Host "$LambdaChar" -nonewline -foreground White }
$PromptSuffixes['VisualStudio'] = { write-host " $VisualStudioChar " -nonewline -foregroundcolor White -backgroundcolor Magenta }

# A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
# doesn't play nice with other things. 
$colorPrompt = {

    # Useful with ConEmu's status bar's "Console Title" field - always puts your CWD in the status bar
    $Host.UI.RawUI.WindowTitle = $pwd

    Write-Host $(get-date -format HH:mm:ss) -nonewline -foregroundcolor White

    #if ($errorActionPreference -ne "Stop" -and $error.count -gt 0) {
    write-host " E$($error.count)" -nonewline -foreground red
    #}

    Write-Host " $hostname" -nonewline -foregroundcolor Blue

    $jobs = get-job
    #write-host ' {' -nonewline
    write-host " J$($jobs.count)" -nonewline -foreground (Get-JobStateColor $jobs.State)
    #write-host '}' -nonewline

    Write-Host " $(Get-DisplayPath $pwd) " -nonewline -foregroundcolor Green
    
    # This lets you define a $promptSuffix scriptblock variable elsewhere.
    # I use this for my DLP SolutionScripts.profile.ps1 for example. 
    if ($PromptSuffixes['Override']) {
        invoke-command $PromptSuffixes['Override']
    }
    elseif ($VisualStudioDirectories |? {"$pwd".StartsWith($_)}) {
        invoke-command $PromptSuffixes['VisualStudio']
    }
    elseif ($SoyAdmin) {
        invoke-command $PromptSuffixes['Admin']
    }
    else {
        invoke-command $PromptSuffixes['Default']
    }

    # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
    return " "
}

# A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
$simplePrompt = {
    if ($SoyAdmin) { $lcop = "#" }
    else { $lcop = ">" }
    return "$(get-date).Tostring('HH:mm:ss') $hostname $(Get-DisplayPath $pwd) PS$lcop "
}

function reset-prompt {
    if (test-path function:prompt) { rm function:prompt }
    new-item -path function:prompt -value $colorPrompt | out-null
}

. reset-prompt

if ($env:term -eq "emacs") {
    # Emacs' "M-x powershell" seems to handle the prompt itself, and you get extra newlines if you 
    # define one 
    if (test-path function:prompt) { rm function:prompt }
}


