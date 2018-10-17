<#
.SYNOPSIS
Helper functions for setting the prompt
#>

$script:UserPrompts = @{

    # A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
    # doesn't play nice with other things.
    Color = {
        # If the console has been corrupted by e.g. running a command with color output and then pressing
        # ctrl-c while it's printing a color, this reset will ensure the colors do not ruin subsequent lines in the console
        [Console]::ResetColor()

        # Useful with ConEmu's status bar's "Console Title" field - always puts your CWD in the status bar
        $Host.UI.RawUI.WindowTitle = $pwd

        $DoublePrompt = [char]187    #  »  (RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK)
        $HammerSickle = [char]9773   #  ☭  (HAMMER AND SICKLE)

        # Wrap our writes in comments so that we can copy/paste entire lines, including the prompt
        Write-Host '<# ' -NoNewLine -ForegroundColor DarkGray

        Write-Host $(get-date -format HH:mm:ss) -nonewline -foregroundcolor White
        $eColor = if ($global:Error -or $LASTEXITCODE) { "Red" } else { "DarkGray" }
        $lastExitDisplay = if ("$LASTEXITCODE") { $LASTEXITCODE } else { "0" }
        write-host " E:$($global:Error.count):$lastExitDisplay" -nonewline -foreground $ecolor
        Write-Host " $(MrlOsHelper\Get-Hostname)" -nonewline -foregroundcolor Blue
        $jobs = get-job
        if ($jobs) {
            write-host " J$($jobs.count)" -nonewline -foreground (Get-JobStateColor $jobs.State)
        }
        else {
            write-host " J0" -nonewline -foreground White
        }
        Write-Host " $(Get-DisplayPath $pwd) " -nonewline -foregroundcolor Green

        if (MrlOsHelper\Test-AdminRole) {
            # Surround with spaces because this is a wide character
            Write-Host " $HammerSickle " -NoNewLine -ForegroundColor Red -BackgroundColor Yellow
        }
        else {
            Write-Host $DoublePrompt -NoNewLine -ForegroundColor White
        }

        # Close our comment
        Write-Host ' #>' -NoNewLine -ForegroundColor DarkGray

        # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
        return " $(Get-ConEmuPromptMetadata)"
    }

    # A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
    Simple = {
        $lcop = if (MrlOsHelper\Test-AdminRole) {"#"} else {'>'}
        return "$((get-date).Tostring('HH:mm:ss')) $(MrlOsHelper\Get-Hostname) $(Get-DisplayPath $pwd) PS$lcop $(Get-ConEmuPromptMetadata)"
    }

    # A very tiny prompt that at least differentiates based on color
    Tiny = {
        $lcop = if (MrlOsHelper\Test-AdminRole) { "#" } else { ">" }
        write-host "PS$lcop" -foreground Green -nonewline
        return " $(Get-ConEmuPromptMetadata)"
    }

}

function Get-ConEmuPromptMetadata {
    # Indicate end of prompt, allows selecting typed command, change cursor position with click, etc
    $promptEndIndic = "$([char]27)]9;12$([char]7)"

    # Indicate CWD if in a filesystem, allowing restore on restart and hyperlink clicks from git
    $location = Get-Location
    if ($location.Provider.Name -eq "FileSystem") {
        $promptCwd = "$([char]27)]9;9;`"$($location.Path)`"$([char]7)"
    } else {
        $promptCwd = ""
    }
    return "${promptEndIndic}${promptCwd}"
}

function Get-DisplayPath {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] $path
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
    [CmdletBinding()] Param(
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
    [CmdletBinding(DefaultParameterSetName='ByName')] param(
        [Parameter(Position=0, Mandatory=$True, ParameterSetName='ByName')]
        [String] $Name,

        [Parameter(Position=0, Mandatory=$True, ParameterSetName='Custom')]
        [ScriptBlock] $ScriptBlock
    )
    switch ($PsCmdlet.ParameterSetName) {
        'ByName' {
            if ($script:UserPrompts.Keys -NotContains $Name) {
                throw "No registered prompt named '$Name'. Has it been added with Register-UserPrompt?"
            }
            Set-UserPrompt -ScriptBlock $script:UserPrompts[$Name]
        }
        'Custom' {
            # Don't pass the scriptblock as is to Set-Content, but copy the script block
            # This ensures that if we remove the MrlPromptHelper module, the prompt stays in memory
            Set-Content -Path Function:\prompt -Value $([ScriptBlock]::Create("$ScriptBlock")) | Out-Null
        }
        default {
            throw "Unknown parameter set $($PsCmdlet.ParameterSetName)"
        }
    }
}

function Register-UserPrompt {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name,
        [Parameter(Mandatory)] [string] $ScriptBlock
    )
    $script:UserPrompts[$Name] = $ScriptBlock
}

function Unregister-UserPrompt {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name
    )
    $script:UserPrompts.Remove($Name)
}

function Get-UserPrompt {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name
    )
    return $script:UserPrompts[$Name]
}

function Get-UserPromptList {
    [CmdletBinding()] Param()
    return $script:UserPrompts.Keys
}