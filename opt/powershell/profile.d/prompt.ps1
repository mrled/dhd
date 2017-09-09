﻿function Get-DisplayPath {
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


function Set-UserPrompt {
    [CmdletBinding(DefaultParameterSetName='BuiltIn')] param(
        [Parameter(Position=0, ParameterSetName='BuiltIn')] [ValidateSet('Color','Simple','Tiny')] [String] $builtInPrompt,
        [Parameter(Position=0, Mandatory=$True, ParameterSetName='Custom')] [Scriptblock] $newPrompt
    )

    $psCore = $PSVersionTable.Keys -Contains 'PSEdition' -and $PSVersionTable.PSEdition -eq "Core"
    $psPlatform = {
        if ($psCore) {
            return $PsVersionTable.Platform
        } else {
            return "Windows"
        }
    }.Invoke()
    $psAdmin = {
        if ($psPlatform -eq "Windows") {
            $principal = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
            return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
        } else {
            return ((id -u) -eq 0)
        }
    }.Invoke()

    $builtIns = @{
        # A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
        # doesn't play nice with other things.
        Color = {
            # If the console has been corrupted by e.g. running a command with color output and then pressing
            # ctrl-c while it's printing a color, this reset will ensure the colors do not ruin subsequent lines in the console
            [Console]::ResetColor()

            # Useful with ConEmu's status bar's "Console Title" field - always puts your CWD in the status bar
            $Host.UI.RawUI.WindowTitle = $pwd

            Write-Host $(get-date -format HH:mm:ss) -nonewline -foregroundcolor White
            $eColor = if ($error -or $LASTEXITCODE) { "Red" } else { "DarkGray" }
            $lastExitDisplay = if ("$LASTEXITCODE") { $LASTEXITCODE } else { "0" }
            write-host " E:$($error.count):$lastExitDisplay" -nonewline -foreground $ecolor
            Write-Host " $env:COMPUTERNAME" -nonewline -foregroundcolor Blue
            $jobs = get-job
            if ($jobs) {
                write-host " J$($jobs.count)" -nonewline -foreground (Get-JobStateColor $jobs.State)
            }
            else {
                write-host " J0" -nonewline -foreground White
            }
            Write-Host " $(Get-DisplayPath $pwd) " -nonewline -foregroundcolor Green

            if ($psAdmin) {
                Write-Host " $($SpecialCharacters.HammerSickle) " -NoNewLine -ForegroundColor Red -BackgroundColor Yellow
            }
            else {
                Write-Host $SpecialCharacters.DoublePrompt -NoNewLine -ForegroundColor White
            }

            # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
            return " "
        }

        # A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
        Simple = {
            $lcop = if ($psAdmin) {"#"} else {'>'}
            return "$((get-date).Tostring('HH:mm:ss')) $env:COMPUTERNAME $(Get-DisplayPath $pwd) PS$lcop "
        }

        # A very tiny prompt that at least differentiates based on color
        Tiny = {
            $lcop = if ($psAdmin) { "#" } else { ">" }
            write-host "PS$lcop" -foreground Green -nonewline
            return " "
        }
    }

    if ($PsCmdlet.ParameterSetName -eq 'BuiltIn') {
        if (-not $builtInPrompt) {
            # If a specific prompt name wasn't passed, set a default one
            # Because of the double prompt bug under Core on non-Windows platforms, if we aren't running on Windows, we use the Simple prompt by default
            if ($psPlatform -eq 'Windows') {
                Set-UserPrompt -newPrompt $builtIns['Color']
            } else {
                Set-UserPrompt -newPrompt $builtIns['Simple']
            }
        } else {
            Set-UserPrompt -newPrompt $builtIns[$builtInPrompt]
        }
    } else {
        New-Item -Force -Path Function:\prompt -Value $newPrompt | Out-Null
    }
}

<#
.description
Useful in case one of those fucking color commands perma fucks with your background/foreground color. These are my preferences. Get the default values by launching Powershell -NoProfile and examining $Host.UI.RawUI and $Host.PrivateData
#>
function Set-ConsoleColors {
    [CmdletBinding()] Param(
        $BackgroundColor = 'Black',
        $ForegroundColor = 'White',
        $ErrorForegroundColor = 'Red',
        $ErrorBackgroundColor = 'Black',
        $WarningForegroundColor = 'Magenta',
        $WarningBackgroundColor = 'Black',
        $DebugForegroundColor = 'Yellow',
        $DebugBackgroundColor = 'Black',
        $VerboseForegroundColor = 'Green',
        $VerboseBackgroundColor = 'Black',
        $ProgressForegroundColor = 'DarkBlue',
        $ProgressBackgroundColor = 'White'
    )
    [Console]::ResetColor()
    $Host.UI.RawUI.BackgroundColor = $BackgroundColor
    $Host.UI.RawUI.ForegroundColor = $ForegroundColor
    $Host.PrivateData.ErrorForegroundColor = $ErrorForegroundColor
    $Host.PrivateData.ErrorBackgroundColor = $ErrorBackgroundColor
    $Host.PrivateData.WarningForegroundColor = $WarningForegroundColor
    $Host.PrivateData.WarningBackgroundColor = $WarningBackgroundColor
    $Host.PrivateData.DebugForegroundColor = $DebugForegroundColor
    $Host.PrivateData.DebugBackgroundColor = $DebugBackgroundColor
    $Host.PrivateData.VerboseForegroundColor = $VerboseForegroundColor
    $Host.PrivateData.VerboseBackgroundColor = $VerboseBackgroundColor
    $Host.PrivateData.ProgressForegroundColor = $ProgressForegroundColor
    $Host.PrivateData.ProgressBackgroundColor = $ProgressBackgroundColor
}

function Test-ProgressBar {
    for ($i = 0; $i -lt 100; $i+=20 ) {
        Write-Progress -Activity "Test in progress" -Status "$i% Complete:" -PercentComplete $i
        Start-Sleep 1
    }
}
