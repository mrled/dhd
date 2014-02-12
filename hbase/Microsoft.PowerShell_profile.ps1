# -*- mode: powershell -*-

$hostname=[System.Net.Dns]::GetHostName()

$Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# $profile is actually a PS object. $profile|get-member shows other NoteProperty entries that may be of interest
# After this line you can do $profile.dhd to get the path to this file.
$profile | Add-Member -MemberType NoteProperty -Name "dhd" -Value $myinvocation.mycommand.path -force

# You have to set the EAP to "stop" if you want try/catch to do anything, so...
# I want it to be stop anyway (I think?) but I'll save the default here just in case.
$default_eap = $ErrorActionPreference
$ErrorActionPreference = "stop"
set-psdebug -strict # throw an exception for variable reference before assignment 
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


# fuck this path
$adkpath =  "${env:programfiles(x86)}\Windows Kits\8.0\Assessment and Deployment Kit\Deployment Tools\${env:Processor_Architecture}\DISM"
if (test-path $adkpath) { $env:PSModulePath = $env:PSModulePath + ";$adkpath" }

# TODO: function to only add these if they exist
$env:PSModulePath = $env:PSModulePath + ";$home\.dhd\opt\powershell\modules"
$env:PSModulePath = $env:PSModulePath + ";$home\Documents\WindowsPowerShell\Modules"
import-module IPConfiguration,uPackageManager
try {
    # Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file, 
    # so import the module before adding that format file so my format file overrides their bullshit
    import-module PsGet,PSCX,posh-git 
}
catch {}

foreach ($um in (gci ~/.dhd/opt/powershell/umodules)) {
    . $um.fullname
}

# override some default display values for objects, this feature ruelz
update-formatdata -prependpath "$home\.dhd\opt\powershell\mrl.format.ps1xml"

$profiled = "$home\.dhd\opt\powershell\profile.d"

. $profiled\initialization.ps1
. $profiled\prompt.ps1


function Enable-Readline {
    import-module PSReadline # you have to define the prompt before importing this

    set-psreadlineoption -editmode emacs

    # A way to see possible handler methods is: 
    # [PSConsoleUtilities.PSConsoleReadline].GetMethods().Name

    #Set-PSReadlineKeyHandler -key Ctrl+R -function HistorySearchBackward
    #Set-PSReadlineKeyHandler -key Ctrl+S -function HistorySearchForward

    # this doesn't work at all? 
    #$rlhandler = { [PSConsoleUtilities.PSConsoleReadLine]::RevertLine() }
    #Set-PSReadlineKeyHandler -Key Ctrl+C -BriefDescription RevertLine -Handler $rlhandler

    Set-PSReadlineKeyHandler -key Ctrl+P -function PreviousHistory
    Set-PSReadlineKeyHandler -key Ctrl+N -function NextHistory
}

# You must enable readline after setting your prompt for it to work correctly
enable-readline

# Note that this adds it to your Powershell history but not your command prompt history :(
$historyfile = "$profile" -replace "_profile.ps1","_history.csv"

$historyExitEvent = {
    if (test-path $historyfile) {

        $shellhist = get-history -count 1000

        # only get shell history that has occurred since we added history from the hist file
        for ( $i = $shellhist.length; $i -ge 0; $i--) {
            if ($shellhist[$i].id -eq $global:finalFileHistoryId) {
                break;
            }
        }
        $newshellhist = $shellhist[$i..$shellhist.length]


        # we get the file history again so that we don't clobber history added by another
        # exiting shell. 
        clear-history
p        import-csv $historyfile | add-history
        $newshellhist | add-history 
    }

    get-history | export-csv $historyfile
}

$historyStartupEvent = {
    if (test-path $historyfile) {
        import-csv $historyfile | add-history
        $global:finalFileHistoryId = (get-history)[-1].id
    }
    else {
        $global:finalFileHistoryId = 0
    }
}

# These slow down the exit process considerably, and don't work with PSRealine's built-in backwards history stuff 
# (at least for the moment), so not worth it. 
#Register-EngineEvent Powershell.Exiting $historyExitEvent -SupportEvent
#& $historyStartupEvent
set-alias hist get-history


