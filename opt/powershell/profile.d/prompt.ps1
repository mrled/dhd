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
        return 'DarkGray'
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

# Used to set things like prompt suffix and tab title prefix
$CliContextClues = @{
    Admin = @{
        TitlePrefix = $HammerAndSickleChar
        PromptSuffix = { write-host " $HammerAndSickleChar " -nonewline -foregroundcolor red -backgroundcolor yellow }
    }
    Default = @{
        TitlePrefix = $LambdaChar
        PromptSuffix = { Write-Host "$LambdaChar" -nonewline -foreground White }
    }
    VisualStudio = @{
        TitlePrefix = $VisualStudioChar
        PromptSuffix = { write-host " $VisualStudioChar " -nonewline -foregroundcolor White -backgroundcolor Magenta }
    }
}

function Get-CliContextClue {
    if ($CliContextClues['Override']) {
        return $CliContextClues[$Override]
    }
    elseif ($VisualStudioDirectories |? {"$pwd".StartsWith($_)}) {
        return $CliContextClues['VisualStudio']
    }
    elseif ($SoyAdmin) {
        return $CliContextClues['Admin']
    }
    else {
        return $CliContextClues['Default']
    }
}

set-alias ConEmuC "${env:ConEmuBaseDir}\ConEmuC.exe"

function Set-ConEmuTabTitleForCliContext {
    [cmdletbinding()] param(
        $title
    )
    $clue = Get-CliContextClue
    if ($title) { $title = ":$title" }
    $fullTitle = "$($clue.TitlePrefix)$title" -replace " ",'' 
    $macro = "Rename(0,$fullTitle"
    write-verbose "Running macro: $macro"
    $out = conemuc /guimacro $macro
    if ($out -ne "OK") {
        throw "Failed to change tab title with error: $out"
    }
}
set-alias Rename-Tab Set-ConEmuTabTitleForCliContext

# A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
# doesn't play nice with other things. 
$colorPrompt = {

    # Useful with ConEmu's status bar's "Console Title" field - always puts your CWD in the status bar
    $Host.UI.RawUI.WindowTitle = $pwd

    Write-Host $(get-date -format HH:mm:ss) -nonewline -foregroundcolor White

    #if ($errorActionPreference -ne "Stop" -and $error.count -gt 0) {
    if ($error -or $LASTEXITCODE) { $ecolor = "Red" }
    else { $ecolor = "DarkGray" }
    write-host " E:$($error.count):$LASTEXITCODE" -nonewline -foreground $ecolor
    #}

    Write-Host " $hostname" -nonewline -foregroundcolor Blue

    $jobs = get-job
    #write-host ' {' -nonewline
    write-host " J$($jobs.count)" -nonewline -foreground (Get-JobStateColor $jobs.State)
    #write-host '}' -nonewline

    Write-Host " $(Get-DisplayPath $pwd) " -nonewline -foregroundcolor Green
    
    # This lets you define a $promptSuffix scriptblock variable elsewhere.
    # I use this for my DLP SolutionScripts.profile.ps1 for example. 
    $clue = Get-CliContextClue
    invoke-command $clue.PromptSuffix

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


