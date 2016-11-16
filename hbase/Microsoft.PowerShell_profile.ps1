if (-not $profile) {
    # This can happen in a remote session, for example
    $profile = New-Object PSObject -Property @{
        CurrentUserCurrentHost = "$Home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"
    }
    Add-Member -InputObject $profile -MemberType ScriptMethod -Name ToString -Value {$this.CurrentUserCurrentHost} -Force
}

Add-Member -InputObject $profile -MemberType NoteProperty -Name "ProfileD" -Value "$home\.dhd\opt\powershell\profile.d" -Force


if ($osType -match "Windows") {
    $Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    $SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}
elseif ($osType -match "Unix") {
    $SoyAdmin = (id -u) -eq 0
}

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

1.  I rely on some external modules like PSCX, PSReadline, and others
    a.  I need Chocolatey for lots of things, but in particular: PsGet
    b.  I need PsGet for at least PSReadline
    c.  I need PSReadline because duh
2.  I have my own modules in .dhd/opt/powershell/modules.
    These are totally self-contained
3.  I have other scripts in .dhd/opt/powershell/profile.d
    Stuff in that directory should not rely on being loaded in any specific order
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
Add-Member -force -inputObject $profile -MemberType NoteProperty -Name MrlFormat -Value "$($profile.DHDPath)\opt\powershell\mrl.format.ps1xml"
Update-FormatData -prependpath $profile.MrlFormat

Set-UserPrompt

# Must set the prompt before importing this module or it'll puke
Import-Module PSReadline 
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -HistorySaveStyle SaveIncrementally
