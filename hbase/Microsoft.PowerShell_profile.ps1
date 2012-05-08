# -*- mode: powershell -*-
# In order to set this file as your profile, the easiest
# way is to open up PowerShell, type `$profile`, and save the 
# following text in that file: 
#     . $Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1 

# Note that to run commands on remote machines you'll need WinRM
# enabled - it isn't by default. 
# <http://technet.microsoft.com/en-us/magazine/ff700227.aspx>

$hostname=[System.Net.Dns]::GetHostName()

$Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
# $me.identity.{name,user,groups} also potentially useful

$startmenu="$env:appdata\Microsoft\Windows\Start Menu"


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


# via: https://github.com/stephenn/powershell_sudo
# via: http://www.ainotenshi.org/710/%E2%80%98sudo%E2%80%99-for-powershell-sorta
# this works OK for things like "notepad C:\Windows\something.txt"
# it doesn't preserve CWD and other things though
function sudo()
{
    if ($args.Length -eq 1)
    {
        start-process $args[0] -verb "runAs"
    }
    if ($args.Length -gt 1)
    {
        start-process $args[0] -ArgumentList $args[1..$args.Length] -verb "runAs"
    }
}

function .. { cd .. }

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}

# note: 7-zip is in the same place on both 64 bit and 32 bit Windows
# note: in some cases it won't complete commands starting with a digit, so we are reduced to this
set-alias sz "C:\Program Files\7-Zip\7z.exe" 

function prompt {
    Write-Host $(get-date).Tostring("HH:mm:ss") -nonewline -foregroundcolor White
    Write-Host (" " + $hostname) -nonewline -foregroundcolor Blue
    Write-Host (":") -nonewline -foregroundcolor White
    Write-Host (" " + $pwd + " ") -nonewline -foregroundcolor Green
    Write-Host ("PS>") -nonewline -foregroundcolor White
    # Always return a string or PS will echo the standard "PS>" prompt and it will append to yours
    return " "
}

if (test-path "C:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE") {
    $vs2010path="C:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE"
    set-alias devenv "$vs2010path\devenv.exe"
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
        if ($level -eq 0) {write-host ($a) -foregroundcolor Green}
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
        $gcmoutput = get-command $a
        if ($gcmoutput.count) { $cmdobjs = $gcmoutput } #there was an array of results; use it
        else { $cmdobjs += $gcmoutput } #there was just one result; make a one-item array

        foreach ($c in $cmdobjs) {
            if ($c.CommandType) { #sometime get-command passes us an empty object! awesome!!
                switch ($c.CommandType) {
                    "Alias" {
                        write-host ($levelprefix + $c.Name + ": Aliased to " + $c.Definition) #-nonewline
                        if ($recurse.ispresent) {
                            $level = $level +1
                            Display-AllCommands $c.Definition -recurse -recursionlevel $level
                        }
                    }
                    "Application" { 
                        write-host ($levelprefix + $c.Name + ": Executable at " + $c.Definition) 
                    }
                    "Function" {
                        write-host ($levelprefix + $c.Name + ": " + $c.CommandType)
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

                        write-host ($defstr) 
                    }
                    default { write-host ($levelprefix + $c.Name + ": " + $c.CommandType) }
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

#if (gcm less 2> $null) { set-alias l less }
#else { set-alias l more }
set-alias l more # the GnuWin32 less.exe is crashing Console2, ugh.

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

function man {
    foreach ($a in $args) {
        get-help $a -detailed | more
    }
}
if (gci alias:cd 2>&1 > $null) { rm alias:cd }
function cd {
    if ($args.Count -le 0) {
        set-location $home
    }
    else {
        set-location "$args"
    }
}
set-alias nc ncat
function uploadid ($host) {
    $akeys = "~/.ssh/authorized_keys"
    pscp ~/.ssh/id_rsa.pub $host:~/.ssh/
    get-content ~/.ssh/id_rsa.pub `
        | plink $args "mkdir -p ~/.ssh && cat - >> $akeys && chmod 600 $akeys"
}
function youtube-dl {
    python "$home\opt\src\youtube-dl\youtube-dl" $args
}


function Display-Path {
    #$re_semicolon = new-object system.text.regularexpressions.regex ("`;")
    foreach ($pathitem in $env:path.split("`;")) {
        write-host $pathitem
    }
}

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