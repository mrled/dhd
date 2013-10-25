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

# I need Chocolatey for stuff, including psget
# I need psget for stuff, including PSReadline
# I need PSReadline because duh
$env:PSModulePath = $env:PSModulePath + ";$home\.dhd\opt\powershell\modules"
import-module IPConfiguration
import-module uPackageManager
try {
    # Note that PSCX fucks with my get-childitem formatting in my mrl.format.ps1xml file, 
    # so import the module before adding that format file so my format file overrides their bullshit
    import-module PsGet,PSCX
}
catch {}

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

#### PATH SECTION
# todo : could mimic set-variable options as closely aspossible
function Set-EnvironmentVariable {
    param(
        [parameter(mandatory=$true)] [string] $Name,
        [string] $Value = "",
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = "Process"
    )
    foreach ($target in $TargetLocation) {
        [Environment]::SetEnvironmentVariable($Name, $Value, $target)
    }
}
set-alias setenv Set-EnvironmentVariable
# TODO: when getting multiple targets, it outputs an array of strings for the value
#       honestly not sure what to do here. %PATH% is *concatenated*, but others are 
#       *replaced* when there's a user and a machine one. ?????
function Get-EnvironmentVariable {
    param(
        [parameter(mandatory=$true)] [string] $Name,
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = "Process"
    )
    $out = ""
    foreach ($target in $TargetLocation) {
        ([Environment]::GetEnvironmentVariable($Name, $target))
    }
}
set-alias getenv Get-EnvironmentVariable

function Get-SystemPath {
    ($env:path).split(";")
}

$possiblePaths = @(
    "C:\Chocolatey\bin"
    "$home\.dhd\opt\win32bin"
    "$home\opt\win32bin"
    "$home\opt\Console2"
    "$home\opt\SysinternalsSuite"
    "$home\opt\mupdf"
    "C:\opt\strawberry\perl\bin"
    "C:\opt\GnuWin32\bin"
    "C:\opt\GnuWin32\sbin"
    "C:\opt\local\bin"
    "C:\opt\svn\bin"
    "C:\opt\SysinternalsSuite"
    "C:\opt\nirsoft64"
    "C:\opt\nirsoft_package"
    "C:\opt\Console2"
    "C:\opt\UnxUtils\bin"
    "C:\opt\UnxUtils\usr\local\wbin"
    "C:\opt\sqlite"
    "C:\Program Files (x86)\Git\cmd"
    "C:\Program Files\PuTTY"
    "C:\Program Files\7-Zip"
    "C:\Program Files (x86)\7-Zip"
    "C:\Program Files (x86)\PuTTY"
    "C:\Program Files\Windows SDKs\Windows\v7.0\Bin"
    "C:\Program Files\NSIS"
    "C:\Program Files (x86)\NSIS"
    "C:\Program Files\Nmap"
    "C:\Program Files (x86)\Nmap"
    "C:\Program Files (x86)\VMware\VMware Virtual Disk Development Kit\bin"
    "C:\Program Files\VMware\VMware Virtual Disk Development Kit\bin"
)
foreach ($py in (get-item C:\Python*)) {
    $possiblePaths += "$($py.fullname)"
    $possiblePaths += "$($py.fullname)\Tools\Scripts"
}
#$possiblePaths += @([environment]::GetEnvironmentVariable("Path", "User") -split ";")
$possiblePaths += @((getenv path user) -split ';')
$possiblePaths = $possiblePaths | sort -unique
# It doesn't do this by default because it's kinda slow maybe
function Set-Systempath {
    $existingPaths = @()
    foreach ($pp in $possiblePaths) {
        if (test-path $pp) { $existingPaths += @($pp) }
    }

    # Set the path for *future processes*
    # We only set the User path, not the system path.
    # This won't take effect in the current shell
    setenv -name Path -value "$($existingPaths -join ";")" -targetlocation User

    # Set the path for *the current shell*
    # Since this will replace the path variable completely, we also have to go through the system path
    #$existingPaths += @([environment]::GetEnvironmentVariable("Path", "Machine") -split ";")
    $existingPaths += @((getenv path machine) -split ';')
    setenv -name Path -value "$($existingPaths -join ";")" -targetlocation Process
}

#### END PATH SECTION

# Term is important for things like less.exe, sometimes
#set-environmentvariable -name "TERM" -value "msys" -targetlocation user,process

# Set filetype associations
### TODO: these assoc and ftype commands only work when elevated
###       gotta replace them with something real! Probably writing to the registry.
###       http://social.msdn.microsoft.com/Forums/vstudio/en-US/630ed1d9-73f1-4cc0-bc84-04f29cffc13b/
###       essentially: see HKEY_CURRENT_USER\Software\Classes
<#
function assoc { cmd /c assoc $* }
function ftype { cmd /c ftype $* }
assoc .el=txtfile
assoc .nfo=txtfile
assoc .mdwn=txtfile
assoc .markdown=txtfile
assoc .md=txtfile
assoc .text=txtfile
#>

# Python profile path shit
set-environmentvariable -name PYTHONSTARTUP -value "$home\.dhd\hbase\python.profile" -targetlocation user,process

# Python script execution
# Get the latest version installed (assuming the default Python install path)
$pythondir = (get-item c:\python* | sort)[-1]
$pythonexe = "$pythondir\python.exe"
<#
assoc .py=Python.File
ftype "Python.File=$pythonexe" "`"%1`"" %*
#>
<#
if (-not (getenv pathext machine,user).tolower().contains('.py')) {
    $userpe = (getenv pathext user)+".PY" # getenv inserts a ';' for us
    setenv pathext $userpe user
    # TODO: this line isn't working. fix it later. 
    #setenv pathext "$(getenv pathext machine,user)" process
}
#>

# random shit I figured out about pathext:
# - setting a pathext in user overwrites the system pathext rather than being appended to it
# - .cpl is added to the pathext... automatically? after each process starts? 
#   because it's not in the user or machine environment, but it is in the process environment no matter what




# aliases can't take parameters, and functions have different scope than your shell. 
# Therefore, I can't have a ".b" command like I have to re-source my bash profile.
# If you dot source a function which dot sources a file, it uses your shell scope to pull in the functions
# Therefore, you can run ". p" (note the space) and get the same effect. 
# Note: does NOT take into account new home/path vars (it cannot without relaunching PS)
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
    Set-Systempath
}

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
function Exit-CurrentSession {
    exit
}
Set-PSReadlineKeyHandler -key Ctrl+D `
                         -BriefDescription SmartInsertQuote `
                         -LongDescription "Exit PowerShell" `
                         -ScriptBlock { exit }
function Enable-Readline {
    import-module PSReadline # you have to define the prompt before importing this

    set-psreadlineoption -editmode emacs

    # A way to see possible handler methods is: 
    # [PSConsoleUtilities.PSConsoleReadline].GetMethods().Name

    Set-PSReadlineKeyHandler -key Ctrl+R -function HistorySearchBackward
    Set-PSReadlineKeyHandler -key Ctrl+S -function HistorySearchForward

    # this doesn't work at all? 
    #$rlhandler = { [PSConsoleUtilities.PSConsoleReadLine]::RevertLine() }
    #Set-PSReadlineKeyHandler -Key Ctrl+C -BriefDescription RevertLine -Handler $rlhandler

    Set-PSReadlineKeyHandler -key Ctrl+P -function PreviousHistory
    Set-PSReadlineKeyHandler -key Ctrl+N -function NextHistory
}

# You must do this after setting your prompt for it to work correctly
enable-readline

$sublpath = "C:\Program Files\Sublime Text 3\sublime_text.exe"
if (test-path $sublpath) {
    #set-alias subl "$sublpath"
    function subl {
        $files = @()
        foreach ($f in $input) { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
        foreach ($f in $args)  { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
        start-process $sublpath -argumentlist $files
        #write-host $files
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


