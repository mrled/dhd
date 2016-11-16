<#
.synopsis
My Powershell profile
.notes
- Copy .dhd\hbase\Microsoft.Powershell_profile.win32.ps1 to $profile.CurrentUserCurrentHost (typically $Home\Documents\WindowsPowerShell\Microsoft.Powershell_profile.ps1)
- That file will dot-source this file
- This file will load required third party modules, required modules in .dhd/opt/powershell/modules, and finally other scripts in .dhd/opt/powershell/profile.d
#>

$ErrorActionPreference = "Stop"

if (-not $profile) {
    # This can happen in a remote session, for example
    $profile = New-Object PSObject -Property @{
        CurrentUserCurrentHost = "$Home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"
    }
    Add-Member -InputObject $profile -MemberType ScriptMethod -Name ToString -Value {$this.CurrentUserCurrentHost} -Force
}

Add-Member -InputObject $profile -MemberType NoteProperty -Name "ProfileD" -Value "$home\.dhd\opt\powershell\profile.d" -Force

# $profile is actually a PS object. $profile|get-member shows other NoteProperty entries that may be of interest
# After this line you can do $profile.DHDProfile to get the path to this file.
$profile | Add-Member -MemberType NoteProperty -Name "DHDProfile" -Value $myinvocation.mycommand.path -force
$profile | Add-Member -MemberType NoteProperty -Name "ConEmu" -Value "$env:AppData\ConEmu.xml" -force

$PossibleModulePaths = @(
    "${env:programfiles(x86)}\Windows Kits\8.0\Assessment and Deployment Kit\Deployment Tools\${env:Processor_Architecture}\DISM"
    "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows Azure\PowerShell\Azure"
    "${env:ProgramFiles(x86)}\Microsoft SQL Server\110\Tools\PowerShell\Modules"
    "$home\.dhd\opt\powershell\modules"
    "$home\Documents\WindowsPowerShell\Modules"
)
foreach ($pmp in $PossibleModulePaths) {
    if (test-path $pmp) { $env:PSModulePath += ";$pmp" }
}

$modules = @(
    # Third party modules:
    'PsGet'
    'posh-git'

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
Add-Member -force -inputObject $profile -MemberType NoteProperty -Name MrlFormat -Value "$Home\.dhd\opt\powershell\mrl.format.ps1xml"
Update-FormatData -prependpath $profile.MrlFormat

Set-UserPrompt

# Must set the prompt before importing this module or it'll puke
Import-Module PSReadline 
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -HistorySaveStyle SaveIncrementally
