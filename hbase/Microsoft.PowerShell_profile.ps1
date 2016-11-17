<#
.synopsis
My Powershell profile
.notes
- Copy .dhd\hbase\Microsoft.Powershell_profile.win32.ps1 to $profile.CurrentUserCurrentHost (typically $Home\Documents\WindowsPowerShell\Microsoft.Powershell_profile.ps1)
- That file will dot-source this file
- This file will load required third party modules, required modules in .dhd/opt/powershell/modules, and finally other scripts in .dhd/opt/powershell/profile.d
#>

$ErrorActionPreference = "Stop"

$newProfileProperties = @{
    ProfileD = "$Home\.dhd\opt\powershell\profile.d"
    MrlFormat = "$Home\.dhd\opt\powershell\mrl.format.ps1xml"
    DHDProfile = $MyInvocation.MyCommand.Path
    ConEmu =  "$env:AppData\ConEmu.xml"
}
Add-Member -NotePropertyMembers $newProfileProperties -InputObject $profile -Force

$PossibleModulePaths = @(
    "${env:programfiles(x86)}\Windows Kits\8.0\Assessment and Deployment Kit\Deployment Tools\${env:Processor_Architecture}\DISM"
    "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows Azure\PowerShell\Azure"
    "${env:ProgramFiles(x86)}\Microsoft SQL Server\110\Tools\PowerShell\Modules"
    "$home\.dhd\opt\powershell\modules"
    "$home\Documents\WindowsPowerShell\Modules"
)
$PossibleModulePaths |% {if (Test-Path $_) {$env:PSModulePath += ";$_"}}

$possibleModules = Get-Module -All
foreach ($optionalModuleName in @("posh-git")) {
    if ($optionalModuleName -in $possibleModules) { Import-Module $optionalModuleName }
}

$modules = @(
    # Third party modules:
    'PsGet'

    # My modules
    'PSWindowsUpdate'
    'ExecutablePathManager'
    'AsciiArt'
)
Import-Module -Name $modules

Show-AsciiSquareWindowsLogo

Get-ChildItem $profile.ProfileD |% { . $_.FullName }

# Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file, 
# so if you're going to use that module, import it first so my format file overrides their bullshit
Update-FormatData -Prependpath $profile.MrlFormat

Set-UserPrompt

# Must set the prompt before importing this module or it'll puke
Import-Module PSReadline 
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -HistorySaveStyle SaveIncrementally
