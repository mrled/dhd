# -*- mode: powershell -*-
# In order to set this file as your profile, the easiest
# way is to open up PowerShell, type `$profile`, and save the 
# following text in that file: 
#     . $Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1 

# Note that to run commands on remote machines you'll need WinRM
# enabled - it isn't by default. 
# <http://technet.microsoft.com/en-us/magazine/ff700227.aspx>

#### BASIC SETUP

$hostname=[System.Net.Dns]::GetHostName()

$Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
# $me.identity.{name,user,groups} also potentially useful

function whoami {
    $me.identity.name
}
function id {
    $output = "" + $me.identity.name + "(" + $me.identity.user.value + ")"
    $output
}

$startmenu="$env:appdata\Microsoft\Windows\Start Menu"

# You have to set the EAP to "stop" if you want try/catch to do anything, so...
# I want it to be stop anyway (I think?) but I'll save the default here just in case.
$default_eap = $ErrorActionPreference
$ErrorActionPreference = "stop"
set-psdebug -strict # throw an exception for variable reference before assignment 
#set-psdebug -off # disable all debugging stuff / reset to "normal" 

#### MODULES
# See what modules are installed on your system with Get-Module -ListAvailable
# After loading a module, see what commands it has: Get-Command -Module <name>

#import-module PSReadline  # You gotta do this after the prompt because it modifies it

if ($psversiontable.psversion.major -ge 3) {
    # fuck this path
    $adkpath =  "${env:programfiles(x86)}\Windows Kits\8.0\Assessment and Deployment Kit"
    $adkpath += "\Deployment Tools\${env:Processor_Architecture}\DISM"
    if (test-path $adkpath) { import-module $adkpath }
}
$metap_path = "$home\.dhd\opt\powershell\lib\MetaProgramming"
if (test-path $metap_path) {
    # http://blogs.msdn.com/b/powershell/archive/2009/01/04/extending-and-or-modifing-commands-with-proxies.aspx
    import-module $metap_path
}

# override some default display values for objects, this feature ruelz
# TODO: in PS3.0 you can run this multiple times and it doesn't care... dunno if it ignores or reloads, I think reloads. 
#       in PS2.0 it gives an error after the first time. 
#       figure out how to unload it and then check again.
update-formatdata -prependpath "$home\.dhd\opt\powershell\lib\mrl.format.ps1xml"


#### EVERYTHING ELSE

# aliases can't take parameters (wtf), and functions have different scope than your shell. 
# Therefore, I can't have a ".b" command like I have to re-source my bash profile.
# If you dot source a function which dot sources a file, it uses your shell scope to pull in the functions
# Therefore, you can run ". p" (note the space) and get the same effect. 
# Note: does NOT take into account new home/path vars (it cannot without relaunching PS)
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
}

function resolve-hostname {
    foreach ($a in $args) {
        [System.Net.Dns]::Resolve($a).AddressList
    }
}

# reinit: re-set home and path vars from my batch file & launch a new powershell. 
# this reinit will launch a new PS but stay alive in the background until the new one exists. works in Console. 
function reinit {
    & "$Home\.dhd\opt\win32\home-and-path-vars.bat"
    start-process "powershell.exe" -NoNewWindow -Wait
    exit
}
# this reinit launches a new PS in the same window, and exits current PS immediately; doesn't work with Console
function reinit2 {
    & "$Home\.dhd\opt\win32\home-and-path-vars.bat"
    start-process "powershell.exe" -NoNewWindow
    exit
}

function Send-Notification {
    # We use start-job so that the function can return right away, but also sleep for $seconds
    # before removing the icon from the systray. $objNotifyIcon.ShowBaloonTip() returns immediately
    # and the icon remains even after $seconds, so I needed a way to sleep, but I didn't want it
    # to lock my PS session while it did so. Anyway.
    $sb = {
        param(
            [parameter(mandatory=$true)][string]$message,
            [string]$title="Powershell Notification",
            [ValidateSet("Info","Warning","Error")][string]$icon="Info",
            [int32]$seconds=10
        )

        [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")
        $objNotifyIcon = New-Object System.Windows.Forms.NotifyIcon 
        #systray icon - make this customizable too? It is required but that path doesn't look universal.
        $objNotifyIcon.Icon = "C:\Windows\Installer\{3156336D-8E44-3671-A6FE-AE51D3D6564E}\Icon_app.ico"
        
        $objNotifyIcon.BalloonTipIcon = $icon  #in-balloon icon
        $objNotifyIcon.BalloonTipText = $message
        $objNotifyIcon.BalloonTipTitle = $title
        
        $objNotifyIcon.Visible = $True 
        $objNotifyIcon.ShowBalloonTip($seconds * 1000)
        start-sleep $seconds
        $objNotifyIcon.Visible = $False
        $objNotifyIcon = $null
    }
    $job = start-job -scriptblock $sb -argumentlist @args 

    #return $job #useful for debugging
}

# original version from <http://www.techmumbojumblog.com/?p=39>
# I changed it so it uses invoke-command rather than WMI for remoting
# this means it works only w/ PowerShell 2.0 I think
# neither WMI nor PS remoting enabled by default, but I always enable PS
# remoting on my domains anyway. It does limit it to PS 2.0 though (no XP?)
function Get-InstalledPrograms ($computer = 'localhost') {
	$programs_installed = @{};
    $win32_product = @(invoke-command -computername $computer -scriptblock {get-wmiobject -class 'Win32_Product'});
	foreach ($product in $win32_product) {
        $name = $product.Name;
        $version = $product.Version;
        if ($name -ne $null) {
            $programs_installed.$name = $version;
		}
	}
	return $programs_installed;
}

# mklink isn't an exe - it's a cmd.exe builtin! what the fuck. 
# also note that you cannot do this without elevating the prompt first lolololololol
function mklink {
    echo "(Running mklink from cmd.exe...)"
    cmd /c mklink $args
}


# This works. Caveat: Emacs is iffy for some reason. 
# You can 'elevate-process emacs \somefile.txt' just fine
# You can 'elevate-process notepad "\somefile with spaces.txt"'
# But if you 'elevate-process emacs "\somefile with spaces.txt"', Emacs will fail
# I am not sure why. 
function Elevate-Process {
    param(
        $process,
        [string]$arguments = $args
    )
    $psi = new-object System.Diagnostics.ProcessStartInfo $process;
    $psi.Arguments = $arguments;
    $psi.Verb = "runas";
    $psi.WorkingDirectory = get-location;
    $started = [System.Diagnostics.Process]::Start($psi);
}
set-alias sudo elevate-process

function Export-ConemuConfig {
    param(
        [parameter(mandatory=$true)] [string] $filename,
        [switch] $force
    )
    if ($force.ispresent) {
        reg export "HKCU\Software\ConEmu\.Vanilla" "$filename" /y
    } else {
        reg export "HKCU\Software\ConEmu\.Vanilla" "$filename" 
    }
}

function .. { cd .. }

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}

# note: 7-zip is in the same place on both 64 bit and 32 bit Windows
# note: in some cases it won't complete commands starting with a digit, so we are reduced to this
set-alias sz "$env:programfiles\7-Zip\7z.exe" 


# Checking if I'm running as admin
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal( [Security.Principal.WindowsIdentity]::GetCurrent() ) 
$username = $currentprincipal.Identity.Name
$adminrole = [Security.Principal.WindowsBuiltInRole]::Administrator
if ($currentPrincipal.IsInRole($adminrole)) {
    #(get-host).UI.RawUI.Backgroundcolor="DarkRed"
    #clear-host
    #write-host "Warning: PowerShell is running as an Administrator.`n"
    $admin = $true
}
else {
    $admin = $false
}

# A color prompt that looks like my bash prompt. Colors require write-host, which sometimes
# doesn't play nice with other things. 
function colorPrompt {
    Write-Host $(get-date).Tostring("HH:mm:ss") -nonewline -foregroundcolor White
    Write-Host (" ") -nonewline
    Write-Host ($hostname) -nonewline -foregroundcolor Blue
    
    # if we're on an smb share or something $pwd contains loads of useless bullshit; strip it. 
    # Make some other optimizations for space.
    $mypwd = $pwd
    $mypwd = $mypwd -replace [regex]::Escape("Microsoft.Powershell.Core\FileSystem::"),""
    $mypwd = $mypwd -replace [regex]::Escape($home),"~"
    Write-Host (" " + $mypwd + " ") -nonewline -foregroundcolor Green
    
    if ($admin) {
        Write-Host ("PS#") -nonewline -foregroundcolor White -backgroundcolor Red
    }
    else {
        Write-Host ("PS>") -nonewline -foregroundcolor White
    }
    # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
    return " "
}

# A one-line-only prompt with no colors that uses 'return' rather that 'write-host'
# Useful for at least PSReadline
function simplePrompt {
    $dt = $(get-date).Tostring("HH:mm:ss")
    $hn = [System.Net.Dns]::GetHostName()
    
    # if we're on an smb share or something $pwd contains loads of useless bullshit; strip it. 
    # Make some other optimizations for space.
    $mypwd = $pwd
    $mypwd = $mypwd -replace [regex]::Escape("Microsoft.Powershell.Core\FileSystem::"),""
    $mypwd = $mypwd -replace [regex]::Escape($home),"~"
    
    if ($admin) { $lcop = "#" }
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
    # This is pretty broken right now
    # prompt for PSReadline must be single-line only, without write-host 
    disable-prompt
    enable-simpleprompt
    import-module PSReadline # you have to define the prompt before importing this
}
function Disable-Readline {
    disable-prompt
    remove-module PSReadline
    enable-colorprompt
}

function gcollect {
    [GC]::Collect()
}

if (test-path "C:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE") {
    $vs2010path="C:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE"
    set-alias devenv "$vs2010path\devenv.exe"
}

if (test-path "C:\Program Files (x86)\Notepad++\notepad++.exe") {
	set-alias npp "C:\Program Files (x86)\Notepad++\notepad++.exe"	
	set-alias notepad++ "C:\Program Files (x86)\Notepad++\notepad++.exe"	
	}
	
# Make output of get-command better (more like Unix) for interactive use. 
# NOTE: For aliases, the processing function calls the show function again - this is recursive!
# it's so if you have an alias chain like x->y->z->, where x and y are aliases
# and z is a function, you'll get the whole relationship + the function definition as well. 
function Display-AllCommands {
    param(
        [alias("r")] [switch]$recurse,
        [int]$recursionlevel=0,
        # weird syntax means that if the $recursionlevel isn't specified, 
        # $args[0] doesn't become $recursionlevel:
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    if ($args.Count -le 0) {return}
    #for ($a=0; $a -le $args.count; $a++) {
    foreach ($a in $args) {
        $level = $recursionlevel
        # This next line helps keep track if there is lots of output, but also clutters everything. Hmm. 
        #if ($level -eq 0) {write-host ($a) -foregroundcolor Green}
        if ($level -gt 20) { 
            $errstr  = "Recursion is greater than 20 levels deep. Probably a circular set of aliases? "
            write-error ($errstr)
            return
        }
        $levelprefix = ""
        for ($i=0; $i -le $level; $i++) {
            if ($i -eq $level) { $levelprefix += "-> " }
            else { $levelprefix += "   " }
        }

        $cmdobjs = @()
        $gcmoutput = get-command -all $a
        if ($gcmoutput.count) { $cmdobjs = $gcmoutput } #there was an array of results; use it
        else { $cmdobjs += $gcmoutput } #there was just one result; make a one-item array

        foreach ($c in $cmdobjs) {
            if ($c.CommandType) { #sometime get-command passes us an empty object! awesome!!
                switch ($c.CommandType) {
                    "Alias" {
                        write-output ($levelprefix + $c.Name + ": Aliased to " + $c.Definition) #-nonewline
                        if ($recurse.ispresent) {
                            $level = $level +1
                            Display-AllCommands $c.Definition -recurse -recursionlevel $level
                        }
                    }
                    "Application" { 
                        write-output ($levelprefix + $c.Name + ": Executable at " + $c.Definition) 
                    }
                    "Function" {
                        # TODO: don't display function definition unless I do -recurse
                        # Can I still show just the parameters though? Hmm. 
                        write-output ($levelprefix + $c.Name + ": " + $c.CommandType)
                        $defstr = $c.Definition
                        # $c.Definition is a string. 
                        # - SOMETIMES, it begins w/ a new line. if so, chomp.
                        # - SOMETIMES it ends w/ a new line too; chomp that. 
                        # - Then, add the $levelprefix to the beginning of every line 
                        #   AND to the beginning of the whole string
                        # ending with a newline (chomp that too because write-host inserts one).
                        # additionally, insert the $functionprefix at the beginning of every line
                        # AND at the beginning of the whole string
                        # I try to match both \n and \r\n because I've had it give me BOTH (lol)

                        $re_firstnewline = new-object system.text.regularexpressions.regex `
                            ('\A\r?\n', `
                             [System.Text.RegularExpressions.RegexOptions]::MultiLine)
                        $re_lastnewline = new-object system.text.regularexpressions.regex `
                            ('\Z\r?\n', `
                             [System.Text.RegularExpressions.RegexOptions]::MultiLine)
                        $re_newline = new-object system.text.regularexpressions.regex `
                            ('\r?\n', `
                             [System.Text.RegularExpressions.RegexOptions]::MultiLine)
                        $re_stringbegin = new-object system.text.regularexpressions.regex `
                            ('\A', `
                             [System.Text.RegularExpressions.RegexOptions]::MultiLine)

                        $functionprefix = $levelprefix + "   " #indent the funct definitions a bit further
                        $defstr = $re_firstnewline.replace($defstr, '')
                        $defstr = $re_lastnewline.replace($defstr, '')
                        $defstr = $re_newline.replace($defstr, [environment]::NewLine + $functionprefix)
                        $defstr = $re_stringbegin.replace($defstr, $functionprefix)

                        write-output ($defstr) 
                    }
                    default { write-output ($levelprefix + $c.Name + ": " + $c.CommandType) }
                }
            }
        }
    }
}
set-alias wh display-allcommands

# demonstration:
# - a much-too-complex string of aliases to aliases to aliases...
# - what happens when there's more than one command for a given string.
# - recursion.
# remember that you have to dot-source this bitch
function Setup-TestForWh {
    set-alias ttt___ttt uuu___uuu 
    set-alias uuu___uuu vvv___vvv
    set-alias vvv___vvv WSManHTTPConfig #an exe file in system32 on x64 win7
    set-alias WSManHTTPConfig xxx___xxx
    set-alias xxx___xxx get-authenticodesignature #existing cmdlet
    set-alias get-authenticodesignature zzz___zzz
    set-alias zzz___zzz create-shortcut
    function ttt___ttt { echo "functiontest" }
    function xxx___xxx { echo "functiontest" }
    function ttt___ttt { echo "functiontest" }
    function ttt___ttt { echo "functiontest" }
    function WSManHTTPConfig { echo "hurr's a function"; cmd.exe }
    function get-authenticodesignature { echo "functest"; get-content C:\boot.ini; echo "ZUHH" }
    set-alias aaa___aaa bbb___bbb #recursive
    set-alias bbb___bbb aaa___aaa #recursive
}

$emacsbin = "$Home\opt\emacs-23.4\bin" # this is going to change every time I upgrade Emacs or whatever, ugh
$emacsclient = "$emacsbin\emacsclientw.exe"
$emacsclient_quoted = '"' + $emacsclient + '"' # unixy programs can't deal with backslashes/spaces, so
$runemacs = "$emacsbin\runemacs.exe"
set-alias emacsclient $emacsclient
set-alias runemacs $runemacs
function emacs {
    # If there's already an Emacs session, start emacsclient.exe and connect to it. 
    # If not, start runemacs.exe instead. 
    # No bullshit with two separate Win7 taskbar icons, no persistent DOS window. 
    param(
        [string]$filename
    )
    emacsclient -na $runemacs "$filename"
}
set-alias e emacs
$env:GIT_EDITOR = $emacsclient_quoted
$env:SVN_EDITOR = $emacsclient_quoted
$env:EDITOR = $emacsclient_quoted
$env:VISUAL = $emacsclient_quoted

function Create-Shortcut {
    param(
        [parameter(Mandatory=$true)] [string]$filename,
        [parameter(Mandatory=$true)] [string]$target,
        [string]$arguments = $null,
        [alias("f")] [switch]$force
    )
    if (-not $filename.tolower().endswith(".lnk")) {
        # required, or you'll get an error message and fail. 
        $filename = "$filename.lnk"
    }
    if ((test-path $filename) -and (-not $force.ispresent)) {
        # I don't think I care to check if there's a non-link file named .lnk that we're going to overwrite
        write-error ("Filename $filename already exists; use the -f argument to overwrite.")
        return $null
    }
    $wshshell = New-Object -ComObject WScript.Shell
    $lnk = $wshshell.CreateShortcut($filename)
    $lnk.TargetPath = "$target"
    $lnk.Arguments = "$arguments" #it's ok if this is $null
    $lnk.save()
    return $lnk
}
# Put something in your PATH by creating a .bat file there that calls it (ewwwwww)
# Was gonna use shortcuts for this but guess what, you have to call them with the .lnk at the end. 
# fucking lol. 
function Install-Exe {
    param(
        [parameter(Mandatory=$true)] [string] $exe,
        [string] [alias("name")] $installname,
        [string] [validateset("exe", "python")] $exetype = "exe",
        [switch] $force,
        [switch] $IncludeUninstallers,
        [string] $installdir="$Home\opt\win32bin"
    )
    $pythonexe = "C:\opt\Python32\python.exe" #this is obviously not standard / ideal
    if (-not (test-path $exe)) {
        write-error ("No such file: '$exe'.")
        return
    }
    $fsio = get-item $exe
    $justname = (($fsio.name -replace ("\.lnk$","")) -replace ("\.exe$",""))
    $fullpath = $fsio.fullname

    # Ignore uninstallers
    if (-not ($IncludeUninstallers.ispresent)) {
        if (($fsio.name -eq "unins000.exe") -or ($fsio.name -eq "uninstall.exe")) {
            write-host "Tried to run Install-Exe on an uninstaller executable called $($fsio.fullname), but -IncludeUninstallers switch was not present." -foreground Yellow
            return
        }
    }
                

    if ($installname) {
        $scpath = "$installdir\$installname.bat"
    }
    else { 
        $scpath = "$installdir\$justname.bat"
    }
    
    if (test-path $scpath) { 
        if ($force.ispresent) {
            rm $scpath
        }
        else {
            write-error ("Shortcut path '$scpath' exists, and '-force' was not supplied.")
            return
        }
    }

    mkdir -force $installdir > $null # just in case we're on a new box

    write-host "Installing $fullpath to $scpath..."

    if ($exetype -eq "exe") { 
        # Here we are writing out a .bat file (in ASCII, not the default UTF-8).
        # ascii because: http://bytes.com/topic/net/answers/546745-ef-bb-bf-prepended
        "@ECHO OFF" | out-file $scpath -encoding "ASCII" -append
        "`"$fullpath`" %*" | out-file $scpath -encoding "ASCII" -append
    }
    elseif ($exetype -eq "python") {
        "@ECHO OFF" | out-file $scpath -encoding "ASCII" -append
        "$pythonexe `"$fullpath`" %*" | out-file $scpath -encoding "ASCII" -append
    }
}

function Get-RelativePath
{
    # Return a relative path to a file. Only works if the basepath is in the fullpath. 
    param(
        [parameter(mandatory=$true)] [string] $fullpath,
        [parameter(mandatory=$true)] [string] $basepath
    )
    #$relpath = [system.io.path]::GetFullPath($fullpath).SubString([system.io.path]::GetFullPath($basepath).Length + 1)
    #return $relpath
    return [system.io.path]::GetFullPath($fullpath).SubString([system.io.path]::GetFullPath($basepath).Length + 1)
}

# seperating file/dir hard/soft links, because they're different in windows
# i wanted to autodetect the target so you didn't have to care, but then you 
# couldn't make hard/soft links that point to a nonexistent file. 
# note that this does not apply to shortcuts
# also, softlinks require admin privs (...wtf)
# finally, note that you have to `remove-item -recurse -force` to delete a junction
# and that this does in fact ONLY delete the hardlink not the target, or the files in the target.
# future ideas: http://stackoverflow.com/questions/2311105/test-in-powershell-code-if-a-folder-is-a-junction-point
function Create-Link {
    param(
        [Parameter(ParameterSetName='shortcut',Mandatory=$true)] [alias("c")] [switch]$shortcut, 
        [Parameter(ParameterSetName='shortcut')] [string]$arguments = $null, 
#        [Parameter(ParameterSetName='shortcut')] [alias("f")] [switch]$force, 
        [Parameter(ParameterSetName='fhardlink',Mandatory=$true)] [alias("h")] [switch]$fhardlink, 
        [Parameter(ParameterSetName='fsoftlink',Mandatory=$true)] [alias("s")] [switch]$fsoftlink, 
        [Parameter(ParameterSetName='dhardlink',Mandatory=$true)] [alias("j")] [switch]$dhardlink, 
        [Parameter(ParameterSetName='dsoftlink',Mandatory=$true)] [alias("d")] [switch]$dsoftlink, 
        [Parameter(Mandatory=$true)] [string]$target,
        [Parameter(Mandatory=$true)] [string]$source
    )
    if (test-path $source) {
        write-error "Filename $source already exists." #cannot overwrite - what if it's not a link?
        return $null
    }
    switch ($pscmdlet.parametersetname) {
        "shortcut" { 
            $a = @{filename = $source
                   target = $target
                   arguments = $arguments
                   force = $force 
            }
            create-shortcut @a 
        }
        "fhardlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink /h $source $target" -wait -NoNewWindow
        }
        "fsoftlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink $source $target" -verb "runAs" -wait
        }
        "dhardlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink /j $source $target" -NoNewWindow -wait
        }
        "dsoftlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink /d $source $target" -verb "runAs" -wait
        }
    }
}
function ln {
    param(
        [Parameter(ParameterSetName='type',Mandatory=$true)] [switch]$s, 

        [string]$trg,
        [string]$src
    )
    if ($f.ispresent) {
        create-shortcut -filename $src -target $trg -force
    }
    else {
        create-shortcut -filename $src -target $trg
    }
}

# a hack but it is a pain to remember how to do this every time ugh. 
function Initialize-PuttyRsaKey {
    param ([switch]$NoKeygen)
 
    $sshdir = "$home\.ssh"
    $ppk = "$sshdir\id_rsa.ppk"
    $putty_pub = "$sshdir\id_rsa.ppub"
    $pub = "$sshdir\id_rsa.pub"
    $privkey = "$sshdir\id_rsa"

    write-host "NOTE: This will only work with RSA keys: `n-$privkey, `n-$putty_pub, `n-$ppk, `n-$pub"

    mkdir -f $sshdir > $null
    if (-not $NoKeygen.IsPresent) {
        if ((test-path $ppk) -or (test-path $putty_pub) -or (test-path $ppk) -or (test-path $privkey)) {
            write-error ("None of these files may exist: $ppk, $putty_pub, $pub, and $privkey .")
            return
        }
        
        write-host ("Make sure to save your key as $ppk and $putty_pub .")
        write-host ("Note that you should also export your key as an openssh key to $privkey .")
        start-process puttygen -wait
    }
    if (-not (test-path $ppk) -or -not (test-path $putty_pub) -or -not (test-path $privkey)) {
        write-error ("You must save your files as $ppk, $putty_pub, and $privkey .")
        return
    }

    $pageantlnk_path = "$startmenu\Programs\Startup\pageant.lnk"
    $pageantlnk = create-shortcut($pageantlnk_path)
    $pageantlnk.TargetPath = (get-command pageant).Definition
    $pageantlnk.Arguments = $ppk
    $pageantlnk.Save()
    & (gcm $pageantlnk_path).definition

    $newcontent = Convert-PuttyRsaPubKey($putty_pub)
    add-content -path $pub -value $newcontent
    write-host ("Your pubkey has been saved in openssh format to $pub.")
    # note: i don't echo it at the end because copy/pasting from Win terminals
    # gives you linebreaks which don't work. C/O $pub from your editor instead. 
}

# By default, putty saves pub key files with linebreaks everywhere. Convert them to openssh format. 
function Convert-PuttyRsaPubKey {
    param ([string]$ppbfile)
    $pcontent = get-content $ppbfile
    $newcontent = "ssh-rsa "
    for ($i=2; $i -lt $pcontent.count -1; $i++) {
        $newcontent += $pcontent[$i]
    }
    $comment = "$env:username@$hostname-" + $pcontent[1].split("`"")[1]
    $newcontent += " $comment"
    return $newcontent
}
function EfsEncrypt-File {
    param (
        [alias("r")] [switch]$recurse
    )
    foreach ($a in $args) {
        foreach ($f in get-childitem $a) {
            $f.Encrypt()
        }
    }
}

function head {
    param(
        [int]$n=10,
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    foreach ($file in $args) {
        get-content $file | select-object -first $n
    }
}
function tail {
    param(
        [int]$n=10,
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    foreach ($file in $args) {
        get-content $file | select-object -last $n
    }
}

# 'out-host -paging' is the Powershell pager, but it fucking sucks. it litters the screen with 
# '<SPACE> next page; <CR> next line; Q quit' when you hit enter. It doesn't calculate
# screen height properly. It's done those things since at least PS v2, and continues in v3. 
# Plus, in v3, it throws an exception when you hit 'q' to end the paging, and tells you that
# you hit 'q' to end the paging. what the fuck is that about.
# more.com is "the official workaround". I guess nobody fucking using out-host -paging. lol. 
# 
# OTOH, out-host -paging is the only way to get a pager that behaves like 'less', where it 
# displays information as it becomes available, rather than like 'more', which requires that the
# whole command finishes before it will display any information. UGH.
# 
# Old versions of less.exe from GnuWin32 (this means the one shipping with both pscx and git) 
# won't work either.
# If you're running less.exe from inside console2/conemu and GnuWin32's sh.exe, it works fine.
# If you're running less.exe from inside cmd.exe and powershell.exe or cmd.exe, it works fine.
# But if you're running it from console2/conemu and powershell.exe, it fucking crashes.
# http://sourceforge.net/projects/console/forums/forum/143117/topic/4629708
# 2 data points: v394 has the problem, v436 does NOT. 
# Recent msys has 436.
#
# This tries to solve the problem, but doesn't work for piped data lolwut: 
# http://mow001.blogspot.com/2005/11/enhanced-more-function-for-msh.html
# 
# Lots of stuff (including some of my functions) assume that 'more' is the pager.
#
if (test-path alias:more) { del alias:more }
if (test-path function:more) { del function:more }
if (test-path alias:l) { del alias:l }
#function more { #TODO: support getting stuff from $input and also command line arguments. 
#    $input | out-host -paging
#}
if (test-path "C:\opt\MinGW\msys\1.0\bin\less.exe") {
    set-alias less "C:\opt\MinGW\msys\1.0\bin\less.exe"
    set-alias l less
    set-alias more less
    set-alias m less
}
else {
    set-alias more "$env:windir\system32\more.com"
    set-alias m more
}
#set-alias more "$env:windir\system32\more.com"
#set-alias less more
#set-alias l less

# by defaul, touch is aliased to set-filetime, which doesn't create new empty files. 
if (test-path alias:touch) {del alias:touch}
function touch {
    param([parameter(mandatory=$true)] $file)
    if (test-path $file) {
        set-filetime $file
    }
    else {
        new-item -ItemType file $file
    }

}

$nzbgetdir = "$home\opt\nzbget"
if (test-path $nzbgetdir) {
    set-alias nzbget "$nzbgetdir\nzbget.exe"
}

function llm {
    get-childitem $args | sort-object -property lastwritetime
}
function lse {
    get-childitem $args Get-ChildItem -Include * -Recurse -Force -ErrorAction SilentlyContinue `
        | Where-Object {$_.Attributes -ge "Encrypted"} `
        | Select-Object FullName 
}

function echoexec {
    write-host ("$args")
    invoke-expression "$args"
}


function Echoexec-Expression {
    $expression = ""
    foreach ($a in $args) {
        foreach ($char in " ;{}".tochararray()) {
            if ($a -match "$char") {
                $quoteme = $true
            }
        }
        if ($quoteme) { 
            $expression += "`"$a`" "
        }
        else {
            $expression += "$a "
        }
    }
    write-host ("Echoexec-Expression: #: " + $args.count + "; args: " + $expression)
    invoke-expression "$expression"
}
set-alias echoexec echoexec-expression


if (test-path alias:man) { del alias:man }
function man {
    foreach ($a in $args) {
        get-help $a -full | more
    }
}

if (test-path alias:cd) { del alias:cd }
function cd {
    if ($args.Count -le 0) {
        set-location $home
    }
    else {
        set-location "$args"
    }
}
#if (($env:path).split(";") | gci -filter "ncat.exe") { set-alias nc ncat }
try {
    get-command ncat > $null
    set-alias nc ncat
} catch { continue } 


# convert a key that putty uselessly put out in a bullshit format into the format expected by authorized_keys
# expects an rsa2 key. not sure what happens if this is wrong. probably won't work. 
function Convert-PuttyPublicKey {
    param(
        [parameter(mandatory=$True)]  [string]  $keyfile
    )
    $keydata = get-content $keyfile
    if (-not (($keydata[0].StartsWith("---- BEGIN")) -and ($keydata[-1].StartsWith("---- END"))) ) {
        write-error "Invalid Putty public key file"
        return
    }
    $comment = $keydata[1]
    $newcomment = $comment -replace "Comment: `"","" -replace "`"",""
    $xdata = $keydata[2..($keydata.count-2)]  # get only the key data, no comments or header shit
    $newdata = "ssh-rsa "
    foreach ($l in $xdata) { $newdata += $l } # get rid of linebreaks
    $newdata += " $newcomment"
    return $newdata
}

function uploadid {
    param(
        [parameter(mandatory=$True)]  [alias("host")]  [string]  $hostname,
        [string]  $keyfile="$home\.ssh\id_rsa.pub" 
    )
    $keydata = get-content $keyfile
    write-host "using keyfile $keyfile" -color green
    write-host "key data: $keydata" -color green

    # if its in the putty format, fix it first. 
    if ($keydata.startswith("---- BEGIN")) { 
        $keydata = convert-puttypublickey $keyfile
    }

    $akeys = "~/.ssh/authorized_keys"
    "",$keydata | plink $hostname "mkdir -p ~/.ssh && cat - >> $akeys && chmod 700 ~/.ssh && chmod 600 $akeys"
}


function Display-Path {
    #$re_semicolon = new-object system.text.regularexpressions.regex ("`;")

    # foreach ($pathitem in $env:path.split("`;")) {
    #     write-host $pathitem
    # }

    ($env:path).split(";")
}

function Generate-Password 
{
    param([int]$length=8)
    # From: http://ronalddameron.blogspot.com/2009/09/two-lines-of-powershell-random.html
    $null = [Reflection.Assembly]::LoadWithPartialName("System.Web")
    [System.Web.Security.Membership]::GeneratePassword($length,2)  # 8 bytes long
}
set-alias pwgen generate-password

# Word wrap function, return word wrapped version of passed string
# via: http://blog.wolfplusplus.com/?tag=powershell
function WordWrapStr($str)
{
	# Holds the final version of $str with newlines
	$strWithNewLines = ""
	# current line, never contains more than screen width
	$curLine = ""
	# Loop over the words and write a line out just short of window size
	foreach ($word in $str.Split(" "))
	{
		# Lets see if adding a word makes our string longer then window width
		$checkLinePlusWord = $curLine + " " + $word
		if ($checkLinePlusWord.length -gt (get-host).ui.rawui.windowsize.width)
		{
			# With the new word we've gone over width
			# append newline before we append new word
			$strWithNewLines += [Environment]::Newline
			# Reset current line
			$curLine = ""
		}
		# Append word to current line and final str
		$curLine += $word + " "
		$strWithNewLines += $word + " "
	}
	# return our word wrapped string
	return $strWithNewLines
}


function ConvertTo-Base64($string) {
   $bytes  = [System.Text.Encoding]::UTF8.GetBytes($string);
   $encoded = [System.Convert]::ToBase64String($bytes); 

   return $encoded;
}

function ConvertFrom-Base64($string) {
   $bytes  = [System.Convert]::FromBase64String($string);
   $decoded = [System.Text.Encoding]::UTF8.GetString($bytes); 

   return $decoded;
}

function ftype {
    cmd /c ftype $args
}
function assoc {
    cmd /c assoc $args
}

# not gonna call this grep because it's not usable everywhere grep is usable
# however, you can 'ss expression *.txt' and that works as a replacement for a lot of
# my use of grep
set-alias ss select-string