# -*- mode: powershell -*-

$hostname=[System.Net.Dns]::GetHostName()

$Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

$realProfile = $myinvocation.mycommand.path

# You have to set the EAP to "stop" if you want try/catch to do anything, so...
# I want it to be stop anyway (I think?) but I'll save the default here just in case.
$default_eap = $ErrorActionPreference
$ErrorActionPreference = "stop"
set-psdebug -strict # throw an exception for variable reference before assignment 
#set-psdebug -off # disable all debugging stuff / reset to "normal" 

if ($psversiontable.psversion.major -ge 3) {
    # fuck this path
    $adkpath =  "${env:programfiles(x86)}\Windows Kits\8.0\Assessment and Deployment Kit"
    $adkpath += "\Deployment Tools\${env:Processor_Architecture}\DISM"
    if (test-path $adkpath) { import-module $adkpath }
}

$env:PSModulePath = $env:PSModulePath + ";$home\.dhd\opt\powershell\modules"
import-module IPConfiguration

# umodules/micromodules are just scripts that I'm dot sourcing
# they're not mature or large or discrete enough for modules
foreach ($um in (gci ~/.dhd/opt/powershell/umodules)) {
    . $um.fullname
}

function reimport-module {
    param([parameter(mandatory=$true)] [string] $moduleName)
    $module = get-module $moduleName
    if ($module) {
        write-host "Module is imported. Removing and re-adding."
        remove-module $moduleName
        import-module $module.path
    }
    else {
        write-host "Module was not imported. Trying to add module $modulename..."
        import-module $modulename
    }
}
set-alias reimport reimport-module

# override some default display values for objects, this feature ruelz
# TODO: in PS3.0 you can run this multiple times and it doesn't care... dunno if it ignores or reloads, I think reloads. 
#       in PS2.0 it gives an error after the first time. 
#       figure out how to unload it and then check again.
update-formatdata -prependpath "$home\.dhd\opt\powershell\mrl.format.ps1xml"

# aliases can't take parameters (wtf), and functions have different scope than your shell. 
# Therefore, I can't have a ".b" command like I have to re-source my bash profile.
# If you dot source a function which dot sources a file, it uses your shell scope to pull in the functions
# Therefore, you can run ". p" (note the space) and get the same effect. 
# Note: does NOT take into account new home/path vars (it cannot without relaunching PS)
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
}

# reinit: re-set home and path vars from my batch file & launch a new powershell. 
# this reinit will launch a new PS but stay alive in the background until the new one exists. works in Console. 
function reinit {
    & "$Home\.dhd\opt\win32\home-and-path-vars.bat"
    start-process "powershell.exe" -NoNewWindow -Wait
    exit
}
# this reinit launches a new PS in the same window, and exits current PS immediately; doesn't work with Console
# oh. apparently this DOES work with ConEmu + Powershell 3 on Windows 7 x64. SWEET. 
function reinit2 {
    & "$Home\.dhd\opt\win32\home-and-path-vars.bat"
    start-process "powershell.exe" -NoNewWindow
    exit
}

function .. { cd .. }

# A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
# doesn't play nice with other things. 
function colorPrompt {
    try { $realLASTEXITCODE = $LASTEXITCODE }
    catch [System.Management.Automation.RuntimeException] { $realLASTEXITCODE=$null} 

    Write-Host $(get-date).Tostring("HH:mm:ss") -nonewline -foregroundcolor White
    Write-Host (" ") -nonewline
    Write-Host ($hostname) -nonewline -foregroundcolor Blue
    
    $mypwd = $pwd.providerpath -replace [regex]::Escape($home),"~"
    Write-Host (" " + $mypwd + " ") -nonewline -foregroundcolor Green
    
    if ($SoyAdmin) {
        Write-Host ("PS#") -nonewline -foregroundcolor White -backgroundcolor Red
    }
    else {
        Write-Host ("PS>") -nonewline -foregroundcolor White
    }

    $global:LASTEXITCODE = $realLASTEXITCODE

    # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
    return " "
}

# A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
function simplePrompt {
    $dt = $(get-date).Tostring("HH:mm:ss")
    $hn = [System.Net.Dns]::GetHostName()
    
    # if we're on an smb share or something $pwd contains loads of useless bullshit; strip it. 
    # Make some other optimizations for space.
    $mypwd = $pwd
    $mypwd = $mypwd -replace [regex]::Escape("Microsoft.Powershell.Core\FileSystem::"),""
    $mypwd = $mypwd -replace [regex]::Escape($home),"~"
    
    if ($SoyAdmin) { $lcop = "#" }
    else { $lcop = ">" }
    
    return "$dt $hn $mypwd PS$lcop "
}

if ($env:term -eq "emacs") {
    # Emacs' "M-x powershell" seems to handle the prompt itself, and you get extra newlines if you 
    # define one 
    if (test-path function:\prompt) { del function:\prompt }
}
else {
    function global:prompt { colorPrompt }
}

function Disable-Prompt {
    if (test-path function:\prompt) { del function:\prompt }
}    
function Enable-ColorPrompt { 
    disable-prompt
    function global:prompt { colorPrompt }
}
function Enable-SimplePrompt { 
    disable-prompt
    function global:prompt { simplePrompt }
}
function Enable-Readline {
    import-module PSReadline # you have to define the prompt before importing this

    set-psreadlineoption -editmode emacs

    # A way to see possible handler methods is: 
    # [PSConsoleUtilities.PSConsoleReadline].GetMethods().Name

    $hsbhandler = { [PSConsoleUtilities.PSConsoleReadLine]::HistorySearchBackward() }
    Set-PSReadlineKeyHandler -key Ctrl+R -BriefDescription HistorySearchBackward -Handler $hsbhandler
    $hsfhandler = { [PSConsoleUtilities.PSConsoleReadLine]::HistorySearchForeward() }
    Set-PSReadlineKeyHandler -key Ctrl+S -BriefDescription HistorySearchForeward -Handler $hsfhandler

    # this doesn't work at all? 
    #$rlhandler = { [PSConsoleUtilities.PSConsoleReadLine]::RevertLine() }
    #Set-PSReadlineKeyHandler -Key Ctrl+C -BriefDescription RevertLine -Handler $rlhandler

    $phhandler = { [PSConsoleUtilities.PSConsoleReadLine]::PreviousHistory() }
    Set-PSReadlineKeyHandler -key Ctrl+P -BriefDescription PreviousHistory -Handler $phhandler
    $nhhandler = { [PSConsoleUtilities.PSConsoleReadLine]::NextHistory() }
    Set-PSReadlineKeyHandler -key Ctrl+N -BriefDescription NextHistory -Handler $nhhandler
}

# You must do this after setting your prompt for it to work correctly
enable-readline

# This is used in at least the uPackageManager module
$pythonexe = "C:\Python33\python.exe"

$sublpath = "C:\Program Files\Sublime Text 3\sublime_text.exe"
if (test-path $sublpath) {
    #set-alias subl "$sublpath"
    function subl {
        $files = @()
        foreach ($f in $input) { if (-not [string]::IsNullOrEmpty($f)) { $files += @("$f") } }
        foreach ($f in $args)  { if (-not [string]::IsNullOrEmpty($f)) { $files += @("$f") } }
        start-process $sublpath -argumentlist $files
    }
}
$env:GIT_EDITOR = $sublpath
$env:SVN_EDITOR = $sublpath
$env:EDITOR = $sublpath
$env:VISUAL = $sublpath


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
        import-csv $historyfile | add-history
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

Register-EngineEvent Powershell.Exiting $historyExitEvent -SupportEvent
& $historyStartupEvent
set-alias hist get-history


