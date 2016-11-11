<#
.description
A module for managing the system path
#>

<#
.description
Return an array with one element for each item in the executable path
#>
function Get-ExecutablePath {
    [CmdletBinding()] Param()
    $env:PATH -split ';'
}

<#
.description
Add a directory to the process PATH
#>
function Add-ExecutablePathDirectory {
    [CmdletBinding(DefaultParameterSetName="Append")] Param(
        [Parameter(Mandatory=$True, Position=0)] $path,
        [Parameter(ParameterSetName="Append")] [Switch] $append,
        [Parameter(ParameterSetName="Prepend")] [Switch] $prepend
    )

    # Filter out the new path from the existing PATH
    # In case the new path is already there, doing this wil make sure that we append or prepend properly
    $newPath = Get-SystemPath |? {$_ -NotMatch [RegEx]::Escape($path)}
    if ($PsCmdlet.ParameterSetName -match "append") {
        $newPath = $newPath + @($path)
    }
    elseif ($PsCmdlet.ParameterSetName -match "prepend") {
        $newPath = @($path) + $newPath
    }
    else {
        throw "Don't know how to handle parameter set '$($PsCmdlet.ParameterSetName)'"
    }

    $env:PATH = $newPath -join ';'
}

<#
.description
Replace the current $env:PATH with the value of the System and User - but not Process - PATH environment variable
.notes
Windows has three different types of environment variables: system-level environment variables, which are stored in the registry and require administrator access to change; user-level environment variables, which are stored in the user registry; and process-level environment variables, which are just stored in memory for the current process.

Furthermore, the PATH environment variable is special - whereas normally, a process variables override user variables which override system variables, the process PATH variable is *appended* to the user PATH variable which is *appended* to the system PATH variable.

When a process is started, a copy of the system and user environment variables is made; this becomes the process environment. The process isn't notified if system or user environment variables change. This meansd that if the system or user PATH variable is updated, the current Powershell session won't be able to use commands from the new PATH.

This function throws away the current path and sets it to the concatentated system and user PATH variables.
#>
function Update-ExecutablePathFromEnvironment {
    [CmdletBinding()] Param()
    $env:PATH = "{0};{1}" -f [Environment]::GetEnvironmentVariable("PATH", "Machine"),[Environment]::GetEnvironmentVariable("PATH", "User")
}

<#
.description
Find an item that may be under either the ProgramFiles or ProgramFiles(x86)
.parameter childPath
A subdirectory of either PROGRAMFILES or PROGRAMFILES(X86)
Note that we accept wildcards here, but only the last item (sorted alphabetically) is returned. This lets us do something like "Microsoft Visual Studio *" and have it return only the latest version - so long as versions always ascend alphabetically, anyway.
#>
function Get-ProgramFilesChild {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$True)] $childPath
    )
    if (Test-Path "$env:ProgramFiles\$childPath") {
        Get-Item "$env:ProgramFiles\$childPath" | Select -Last 1
    }
    elseif (${env:ProgramFiles(x86)} -and (Test-Path "${env:ProgramFiles(x86)}\$childPath")) {
        Get-Item "${env:ProgramFiles(x86)}\$childPath" | Select -Last 1
    }
}

<#
.description
Set the executable path to the given value
.parameter path
A string path
.parameter target
The target location for setting the path. "Machine" means persisted system-wide (and requires administrative rights); "User" means persisted user-wide; "Process" means just for this process and its children, not persisted.
#>
function Set-ExecutablePath {
    [CmdletBinding()] Param(
        [String] $path = $env:PATH,
        [ValidateSet("Machine", "User", "Process")] $target = "Process"
    )
    [Environment]::SetEnvironmentVariable("PATH", $path, $target)
}

<#
.description
Find the location of a command in the PATH
#>
function Get-CommandInExecutablePath {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$True)] $commandName
    )
    $pathExt = ($env:PATHEXT -split ';') + '' # Add an empty extension in case the command has no extension or the extension was passed
    foreach ($path in (Get-ExecutablePath)) {
        foreach ($ext in $pathExt) {
            if (Test-Path "$path\${commandName}${ext}") {
                return "$path\${commandName}${ext}"
            }
        }
    }
}
