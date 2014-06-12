# -*- mode: powershell -*-

$profiled = "$home\.dhd\opt\powershell\profile.d"
. $profiled\asciiart.ps1
Show-Metroid

$hostname=[System.Net.Dns]::GetHostName()

$Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# $profile is actually a PS object. $profile|get-member shows other NoteProperty entries that may be of interest
# After this line you can do $profile.dhd to get the path to this file.
$profile | Add-Member -MemberType NoteProperty -Name "dhd" -Value $myinvocation.mycommand.path -force
$profile | Add-Member -MemberType NoteProperty -Name "ConEmu" -Value "$env:AppData\ConEmu.xml" -force
$dhdbase = resolve-path "$(split-path $myinvocation.mycommand.path)\.."

# You have to set the EAP to "stop" if you want try/catch to do anything, so...
# I want it to be stop anyway (I think?) but I'll save the default here just in case.
$default_eap = $ErrorActionPreference
$ErrorActionPreference = "stop"

#set-psdebug -strict # throw an exception for variable reference before assignment 
#set-psdebug -off # disable all debugging stuff / reset to "normal" 

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
    "C:\Projects\DLP\DLPLogisticsModules"
)
foreach ($pmp in $PossibleModulePaths) {
    if (test-path $pmp) { $env:PSModulePath += ";$pmp" }
}

import-module IPConfiguration,uPackageManager

try { import-module credential-management} catch {}

try {
    # Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file, 
    # so import the module before adding that format file so my format file overrides their bullshit
    import-module PsGet
    import-module posh-git
    #import-module PSCX -args $home\.dhd\opt\powershell\Pscx.UserPreferences.ps1
    import-module PSCX
}
catch {}

foreach ($um in (gci ~/.dhd/opt/powershell/umodules)) {
    . $um.fullname
}

# override some default display values for objects, this feature ruelz
update-formatdata -prependpath "$home\.dhd\opt\powershell\mrl.format.ps1xml"

. $profiled\initialization.ps1
. $profiled\prompt.ps1

$profile | Add-Member -MemberType NoteProperty -Name HistoryFile -Value "$Home\Documents\WindowsPowerShell\history.csv" -force
$historyExitEvent = {
    if (test-path $profile.HistoryFile) {

        $shellhist = get-history -count 1000

        # only get shell history that has occurred since we added history from the hist file
        foreach ($id in $shellhist.length..0) {
            if ($shellhist[$id].id -eq $global:finalFileHistoryId) {
                $earliestCommandThisSession = $id + 1
            }
        }
        $newshellhist = $shellhist[$earliestCommandThisSession..$shellhist.length]


        # we get the file history again so that we don't clobber history added by another exiting shell
        clear-history
        import-csv $profile.HistoryFile | add-history
        $newshellhist | add-history 
    }

    get-history | export-csv $profile.HistoryFile
}
$historyStartupEvent = {
    $histCount = (get-history).count
    $histExists = (test-path $profile.HistoryFile) -and [bool](get-content $profile.HistoryFile) 
    if (($histCount -gt 0) -or (-not $histExists)) {
        # Don't import old history if shell isn't new, or if there is no hist file
        $global:finalFileHistoryId = 0 
    }
    else {
        import-csv $profile.HistoryFile | add-history 
        $global:finalFileHistoryId = (get-history)[-1].id
    }
}

# These slow down the exit process considerably, and don't work with PSRealine's built-in backwards history stuff 
# (at least for the moment), so not worth it. 
Register-EngineEvent Powershell.Exiting $historyExitEvent -SupportEvent
& $historyStartupEvent
set-alias hist get-history

import-module PSReadline # you have to define the prompt & do history stuff before importing this
set-psreadlineoption -editmode emacs
Set-PSReadlineKeyHandler -key Ctrl+P -function PreviousHistory
Set-PSReadlineKeyHandler -key Ctrl+N -function NextHistory

