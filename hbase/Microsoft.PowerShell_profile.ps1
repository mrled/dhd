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
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
    & "$Home\.dhd\opt\win32\home-and-path-vars.bat"
}
function reinit { write-host "You meant to type '. p'. Sorry I can't do this for you! Powershell sucks!" }


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


function whence {
    # Useful because it returns a function definition like bash does
    foreach ($a in $args) {
        $gcm = get-command $a
        $gcm # write it to the terminal
        if ( $gcm.CommandType -eq "Function") {
            write-host ("`nDefinition: ")
            $gcm.Definition
        }
    }
}
set-alias wh whence

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