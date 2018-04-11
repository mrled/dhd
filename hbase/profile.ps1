<#
.SYNOPSIS
My Powershell profile
#>

# Halt on all errors
# By default, Powershell only halts execution on "terminating errors" (from `throw`)
# Setting $ErrorActionPreference to "Stop" will halt execution on "non-terminating errors"
# (from `write-error`) as well
$ErrorActionPreference = "Stop"

# Set separators when displaying an array in a string
# By default, Powershell separates array items with a space, like "1 2 3"
# Setting $OFS to ", " will display them with a comma as well, like "1, 2, 3"
$OFS = ", "

$DhdPath = Join-Path -Path $Home -ChildPath ".dhd"

<#
.SYNOPSIS
Re-source my profile
.NOTES
This can be called with '. p' (note the space), analogous to my '.b' bash function.
This function cannot exist in a module, due to scoping issues
#>
function p {
    [CmdletBinding()] Param()
    $profiles = @(
        $profile.AllUsersAllHosts
        $profile.AllUsersCurrentHost
        $profile.CurrentUserAllHosts
        $profile.CurrentUserCurrentHost
    )
    foreach ($prof in $profiles) {
        if (Test-Path -Path $prof) {
            Write-Verbose -Message "Found profile at '$prof', sourcing..."
            . $prof
        } else {
            Write-Verbose -Message "Would source profile at '$prof', but it does not exist"
        }
    }
}

<#
.synopsis
Add a path to $env:PSModulePath, but only if it exists
#>
function Add-ExtantPathToPsModulePath {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string[]] $Path
    )
    $splitPsmp = $env:PSModulePath -split ';'
    foreach ($pathItem in $Path) {
        if ((Test-Path -LiteralPath $pathItem) -and ($pathItem -NotIn $splitPsmp)) {
            Write-Verbose "Adding '$pathItem' to `$env:PSModulePath"
            $env:PSModulePath = @($env:PSModulePath, $pathItem) -join [System.IO.Path]::PathSeparator
        }
    }
}

<#
.synopsis
Import a module, but only if it is installed
#>
function Import-ExtantModule {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string[]] $Name
    )
    $possibleModules = Get-Module -All
    foreach ($possibleModule in $Name) {
        if ($possibleModule -In $possibleModules) {
            Import-Module -Name $possibleModule
        }
    }
}

Add-ExtantPathToPsModulePath -Path @(
    "${env:programfiles(x86)}\Windows Kits\8.0\Assessment and Deployment Kit\Deployment Tools\${env:Processor_Architecture}\DISM"
    "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows Azure\PowerShell\Azure"
    "${env:ProgramFiles(x86)}\Microsoft SQL Server\110\Tools\PowerShell\Modules"
    "$DhdPath/opt/powershell/modules"
    "$home/Documents/WindowsPowerShell/Modules"
)

Import-ExtantModule -Name @(
    "posh-git"
)

Import-Module -Name @(
    'AsciiArt'
    'ExecutablePathManager'
    'MrlInteractive'
    'MrlOsHelper'
    'MrlPromptHelper'
)

Show-AsciiSquareWindowsLogo

foreach ($profileItem in (Get-ChildItem -LiteralPath "$DhdPath/opt/powershell/profile.d")) {
    . $profileItem.FullName
}

# Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file,
# so if you're going to use that module, import it first so my format file overrides their bullshit
Update-FormatData -Prependpath "$DhdPath/opt/powershell/mrl.format.ps1xml"

Set-UserPrompt -Name Color
Set-ConsoleColors

# Must set the prompt before importing this module or it'll puke
Import-Module PSReadline
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -HistorySaveStyle SaveIncrementally
