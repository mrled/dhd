# -*- mode: powershell -*-

if (-not $profile) {
    # This can happen in a remote session, for example
    $profile = New-Object PSObject -Property {
        CurrentUserCurrentHost = "$Home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"
    }
    Add-Member -InputObject $profile -MemberType ScriptMethod -Name ToString -Value {$this.CurrentUserCurrentHost} -Force
}

Add-Member -InputObject $profile -MemberType NoteProperty -Name "ProfileD" -Value "$home\.dhd\opt\powershell\profile.d" -Force
. "$($profile.ProfileD)\asciiart.ps1"
write-host ""; Show-SquareWindowsLogo; write-host ""

$hostname=[System.Net.Dns]::GetHostName()

$Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# $profile is actually a PS object. $profile|get-member shows other NoteProperty entries that may be of interest
# After this line you can do $profile.DHDProfile to get the path to this file.
$profile | Add-Member -MemberType NoteProperty -Name "DHDProfile" -Value $myinvocation.mycommand.path -force
$profile | Add-Member -MemberType NoteProperty -Name "ConEmu" -Value "$env:AppData\ConEmu.xml" -force
$profile | Add-Member -MemberType NoteProperty -Name "DHDPath" -Value "$(split-path $myinvocation.mycommand.path)\.." -Force

# You have to set the EAP to "stop" if you want try/catch to do anything, so...
# I want it to be stop anyway (I think?) but I'll save the default here just in case.
$ErrorActionPreferenceDefault = $ErrorActionPreference
$ErrorActionPreference = "stop"

<# Modules explanation:

1.  I rely on some external modules like PSCX, PSReadline, and others.
    a.  I need Chocolatey for lots of things, but in particular: PsGet
    b.  I need PsGet for at least PSReadline
    c.  I need PSReadline because duh
2.  I have my own modules in .dhd/opt/powershell/modules. 
    These are totally self-contained.
3.  I have "uModules" in .dhd/opt/powershell/umodules. 
    These are actually just scripts that I dot-source. 
    They might grown into large real modules later, or they might stay tiny.
    They should also be completely self-contained.
4.  I have configuration stuff in .dhd/opt/powershell/profile.d.
    Stuff in that directory is loaded in a specific order, and
    may depend on other modules or profile.d files. 
#>

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

import-module IPConfiguration,uPackageManager,PSWindowsUpdate

# If these modules don't exist, don't throw an error
try {
    $errorActionPreferenceCache = $ErrorActionPreference
    $errorCache = $error.ToArray()
    import-module PsGet
    import-module posh-git

    # Add-Member -force -inputObject $profile -MemberType NoteProperty -Name PSCXPreferences -Value "$($profile.DHDPath)\opt\powershell\Pscx.UserPreferences.ps1"
    #import-module PSCX -args $profile.PSCXPreferences # Do I even use any of these functions?
    #import-module PSCX
}
catch {}
finally {
    $error.clear()
    $error.AddRange($errorCache)
    $errorActionPreference = $errorActionPreferenceCache
}

gci ~/.dhd/opt/powershell/umodules |% { . $_.fullname }

# override some default display values for objects, this feature ruelz
# Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file, 
# so import the module before adding that format file so my format file overrides their bullshit
Add-Member -force -inputObject $profile -MemberType NoteProperty -Name MrlFormat -Value "$($profile.DHDPath)\opt\powershell\mrl.format.ps1xml"
update-formatdata -prependpath $profile.MrlFormat

. "$($profile.ProfileD)\initialization.ps1"
. "$($profile.ProfileD)\prompt.ps1"

set-alias hist get-history

import-module PSReadline # you have to define the prompt & do history stuff before importing this
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -HistorySaveStyle SaveIncrementally
