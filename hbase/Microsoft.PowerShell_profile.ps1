# -*- mode: powershell -*-
# In order to set this file as your profile, the easiest
# way is to open up PowerShell, type `$profile`, and save the 
# following text in that file: 
#     . $Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1 

# Note that to run commands on remote machines you'll need WinRM
# enabled - it isn't by default. 
# <http://technet.microsoft.com/en-us/magazine/ff700227.aspx>

$hostname=[System.Net.Dns]::GetHostName()


# aliases can't take parameters (wtf), and functions have different scope than your shell. 
# Therefore, I can't have a ".b" command like I have to re-source my bash profile.
# If you dot source a function which dot sources a file, it uses your shell scope to pull in the functions
# Therefore, you can run ". p" (note the space) and get the same effect. 
# ...
# also I guess I still don't have this right, here's what I need to do: 
# 1) make changes to home-and-path-vars.bat (in a separate editor process)
# 2) install those changes (as I do below)
# 3) pull the installed changes into the current environment (which I'm STILL not doing correctly).
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
    #& "$Home\.dhd\opt\win32\home-and-path-vars.bat"
}
#function reinit { write-host "You meant to type '. p'. Sorry I can't do this for you! Powershell sucks!" }

# OOOOKKKK. This actually does it. 
# It re-sets home and path vars from my batch file
# And the instead of reloading the PowerShell profile it just launches a new PowerShell
# Then it exits when it's done. This sucks more than bash's exec, which outright replaces
# the current shell with the new one, but it seems to work so I'm going with it.
function reinit {
    & "$Home\.dhd\opt\win32\home-and-path-vars.bat"
    & C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
    exit
}

# restarting PowerShell 
# function reinit{
#     & "$Home\.dhd\opt\win32\home-and-path-vars.bat" 
#     $cmd = "Set oShell = CreateObject(""WScript.Shell"") `n"
#     $cmd += "WScript.Sleep 1000 `n"
#     $cmd += "oShell.Run ""PowerShell.exe""" | Out-File -filePath $env:temp"\ps.vbs" -inputobject  
#     $cmd -encoding ASCII -force WScript.exe $env:temp"\ps.vbs"
#     exit
# } 
# function Restart-PowerShell {
#     # the $cmd here-string is passed to VBS, which uses the single-quote as the comment indicator 
#     # (yes, really)
#     $cmd = ""
#     #$cmd += @"'This is a temporary script to Restart a PowerShell Session`n"@
#     #$cmd += @"'Created $(Get-Date)`n"@
#     $cmd += @"Set oShell = CreateObject("WScript.Shell")`n"@
#     $cmd += @"WScript.Sleep 1000`n"@
#     $cmd += @"oShell.Run "PowerShell.exe"`n"@
#     Out-File -filePath $env:temp"\Restart-PowerShell.vbs" -inputobject $cmd -encoding ASCII -force
#     WScript.exe $env:temp"\Restart-PowerShell.vbs"
#     exit
# }
#Set-Alias rsps Restart-PowerShell


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

# mklink isn't an exe - it's a cmd.exe builtin! 
# what the fuck
# also note that if you want to do this without elevating first 
# secpol.msc -> Security Settings -> Local Policies -> User Rights Assignment -> Create symbolic links
# you have to log out after this ahahahahaha fuck
# aaaand just kidding you canNOT do this without elevating first if you're a user that CAN elevate. 
# what. the. actual. fuck. 
function mklink {
    echo "(Running mklink from cmd.exe...)"
    cmd /c mklink $args
}


# via: https://github.com/stephenn/powershell_sudo
# via: http://www.ainotenshi.org/710/%E2%80%98sudo%E2%80%99-for-powershell-sorta
# this works OK for things like "notepad C:\Windows\something.txt"
# it doesn't preserve CWD and other things though
# and it fucking requires that dumb fucking secure desktop bullshit because it's using UAC
# FFFFFFFFFFFFFFFFFFF
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

function .. {
   cd ..
}

# function e {
#     emacsclient -n $args
# }

function trid {
    C:\opt\trid\trid.exe $args
}

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}

function sz {
    # note: 7-zip is in the same place on both 64 bit and 32 bit Windows
    & "C:\Program Files\7-Zip\7z.exe" $args
}


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

# return an array, whether it finds 0, 1, or many results for a command matching 
# $cmdstring in the path.
function Find-CommandObjs-FromPath ($cmdstring) {
    $gcm = get-command $cmdstring
    $output = @()
    if ($gcm.count) { #there was more than one result
        for ($i=0; $i -le $gcm.count; $i++) {
            $output += $gcm[$i]
        }
    }
    else {
        $output += $gcm
    }
    return $output
}

# Make output of get-command better (more like Unix) for interactive use. 
# NOTE: For alias, the processing function calls the show function again - this is recursive!
# it's so if you have an alias chain like x->y->z->, where x and y are aliases
# and z is a function, you'll get the whole relationship + the function definition as well. 
function Display-AllCommands {
    param(
        [switch]$recurse, [switch]$r,
        [int]$recursionlevel=0,
        # weird syntax means that if the $recursionlevel isn't specified, 
        # $args[0] doesn't become $recursionlevel:
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    if ($recurse.IsPresent -or $r.IsPresent) { $recursing = $true }
    else { $recursing = $false }
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

        $cmdobjs = Find-CommandObjs-FromPath ($a)
        foreach ($c in $cmdobjs) {
            if ($c.CommandType) { #sometime get-command passes us an empty object! awesome!!
                switch ($c.CommandType) {
                    "Alias" {
                        write-host ($levelprefix + $c.Name + ": Aliased to " + $c.Definition) #-nonewline
                        if ($recursing) {
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

# TODO: Fixme: accept a -n argument like head and tail on Unix do. 
function head {
    foreach ($file in $args) {
        select-object -first 10
    }
}
function tail {
    foreach ($file in $args) {
        select-object -last 10
    }
}
function more {
    # immediately starts paging output, rather than waiting till the command finishes 
    # before starting to page.
    $input | Out-Host -paging
}