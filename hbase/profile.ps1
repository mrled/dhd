<#
.synopsis
My Powershell profile
.notes
- `Out-File -InputObject ". $Home/.dhd/hbase/profile.ps1" -LiteralPath $profile`
- Then your profile will dot-source
- This file will load required third party modules,
  any required modules in .dhd/opt/powershell/modules,
  and finally other scripts in .dhd/opt/powershell/profile.d
#>

$ErrorActionPreference = "Stop"

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
    "$home/.dhd/opt/powershell/modules"
    "$home/Documents/WindowsPowerShell/Modules"
)

Import-ExtantModule -Name @(
    "posh-git"
)

Import-Module -Name @(
    'ExecutablePathManager'
    'AsciiArt'
)

Show-AsciiSquareWindowsLogo

foreach ($profileItem in (Get-ChildItem -LiteralPath "$Home/.dhd/opt/powershell/profile.d")) {
    . $profileItem.FullName
}

# Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file,
# so if you're going to use that module, import it first so my format file overrides their bullshit
Update-FormatData -Prependpath "$Home/.dhd/opt/powershell/mrl.format.ps1xml"

Set-UserPrompt -builtInPrompt Color
Set-ConsoleColors

# Must set the prompt before importing this module or it'll puke
Import-Module PSReadline
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -HistorySaveStyle SaveIncrementally
