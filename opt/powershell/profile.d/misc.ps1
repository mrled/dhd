#### Miscellaneous / Everything ####


$SpecialCharacters = New-Object PSObject -Property @{
    Beep         = [char]7      # beeps @ u
    DoublePrompt = [char]187    # » (RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK)
    Lambda       = [char]955    # λ (GREEK LETTER LAMBDA)
    HammerSickle = [char]9773   # ☭ (HAMMER AND SICKLE)
    VisualStudio = [char]42479  # ꗯ (VAI SYLLABLE GBE)
}

## Configuring other applications / etc
$gitCommand = Get-CommandInExecutablePath "git"
if ($gitCommand) {

    # $plink = Get-ProgramFilesChild "PuTTY/plink.exe"
    # Removing this means I can use ~/.ssh/config -- so long as a HOME environment variable is set
    # if ($plink) { $env:GIT_SSH = $plink }

    # You want this to be separated with forward slashes so that it works
    # from the Git (bash) command line and cmd and Powershell etc.
    # $env:GIT_EDITOR = "$env:SystemRoot\system32\notepad.exe" -replace "\\","/"

    $sublCommand = Get-CommandInExecutablePath "subl"
    if ($sublCommand) {
        $escapedSublCommand = $sublCommand -replace "\\","/"
        git config --global core.editor "'$escapedSublCommand' -w"
    }

    # Using vim like this as the Git Pager solves some inconsistencies and problems on Windows
    # - Some versions of less that come commonly on Windows are buggy, and it's been hard to figure out the right one to use
    #   For instance, the one that ships with either Chocolatey or Git (idk which) doesn't refresh the screen properly all the time, as of 2017
    #   Many versions of less that are available for Windows do not display colors
    # - Vim has build in support for syntax highlighting unified diffs - which is what "git diff" and "git show" return
    #   Note that this means we turn *off* colors for git diffs if we find vim, because git colors are ANSI escapes and vim doesn't understand those
    $vimCommand = Get-CommandInExecutablePath "vim"
    if ($vimCommand) {
        git config --global color.diff false
        $vimMsysPath = $vimCommand -replace "^(.)\:\\",'\$1\' -replace "\\","/"
        $env:GIT_PAGER = '"{0}" --cmd "let no_plugin_maps = 1" -c "runtime! macros/less.vim" -' -f $vimMsysPath
    }

    # For some reason, on some machines, I have to set GIT_SSH or else Git won't work with any SSH remote
    # I have tested this with Microsoft's OpenSSH port, and it works great, so let's just use that
    $sshCommand = Get-CommandInExecutablePath "ssh"
    if ($sshCommand) {
        $env:GIT_SSH = $sshCommand
    }
}

$env:LESS = "-iRC"

# golang
$goCommand = Get-CommandInExecutablePath "go"
if ($goCommand) {
    if (-not $env:GOROOT) {
        $env:GOROOT = Resolve-Path ((Split-Path -Parent $goCommand) + "/..") | Select -Expand Path
    }
    if (-not $env:GOPATH) {
        $env:GOPATH = "$Home\Documents\Go"
    }
    Add-ExecutablePathDirectory -path "$env:GOROOT\bin"
    Add-ExecutablePathDirectory -path "$env:GOPATH\bin"
}


#### Functions

<#
.description
Retrieve powershell platform (cross platform)
#>
function Get-PowershellPlatform {
    [CmdletBinding()] Param()
    if ($PSVersionTable.Keys -Contains 'Platform') {
        return $PsVersionTable.Platform
    } else {
        # This value is what is returned by $PsVersionTabble.Platform on Powershell Core
        return "Win32NT"
    }
}

<#
.description
Test whether the current session has administrative privileges (cross platform)
#>
function Test-AdminRole {
    [CmdletBinding()] Param()
    if ((Get-PowershellPlatform) -eq "Win32NT") {
        $identity = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
        $adminRole = [Security.Principal.WindowsBuiltInRole] "Administrator"
        return $identity.IsInRole($adminRole)
    } else {
        return (id -u) -eq 0
    }
}

<#
.description
Retrieve the current machine's hostname (cross platform)
#>
function Get-Hostname {
    [CmdletBinding()] Param()
    if ($env:COMPUTERNAME) {
        return $env:COMPUTERNAME
    } else {
        return hostname
    }
}

<#
.synopsis
OH MY GOD
.description
Type a command, change your mind about it, move the cursor to the front of the line, type "omg ", and hit return.
Blammo, it returns "wtf <the command line you typed>"
#>
function omg {
    [cmdletbinding()] param(
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $arguments
    )
    Write-Host "wtf $arguments"
}

function Set-WindowTitle {
    param(
        [parameter(mandatory=$true)] [string] $message
    )
    $Host.UI.RawUI.WindowTitle = $message
}

function Get-PuttySession {
    $regKey = "HKCU:\Software\SimonTatham\PuTTY\Sessions"
    ls $regKey |% { $_.Name -replace "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions\\","" }
}

# These functions are useful because the default colors are far too dark (blue especially)
# And I hate having to click around in the fucking thing just to fucking get the fucking colors so I can fucking see them again.
function Export-PuttySession {
    param(
        [parameter(mandatory=$true)] [string] $filename,
        [string] $sessionName = "Default%20Settings",
        [switch] $force
    )
    $regKey = "HKCU\Software\SimonTatham\PuTTY\Sessions\$sessionName"
    $call = 'reg export $regKey "{0}"' -f "$filename"
    if ($force) { $call += " /y" }
    Invoke-Expression $call
}

function Import-PuttySession {
    param(
        [parameter(mandatory=$true)] [string] $fileName,
        [switch] $force
    )
    $call = 'reg import "{0}"' -f "$fileName"
    if ($force) { $call += " /y" }
    Invoke-Expression $call
}

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}


<#
.synopsis
Start a process and wait for it to exit
.notes
This is a workaround for a stupid idiot bug in start-process that only triggers sometimes.
http://social.technet.microsoft.com/Forums/scriptcenter/en-US/37c1066e-b67f-4709-b195-aa2790216bd0
https://connect.microsoft.com/PowerShell/feedback/details/520554/
The bug has it return instantly even when -wait is passed to start-process, at least on Eric's local box.
When that happens, $process.ExitCode hasn't been populated, and won't be, even when the process does actually exit.
System.Diagnostics.Process doesn't have that behavior, so that's what we're going to use instead
#>
function Invoke-ProcessAndWait {
    [cmdletbinding(DefaultParameterSetName="CheckExitCode")]
    param(
        [parameter(mandatory=$true)] [string] $command,
        [string[]] $argumentList,
        [switch] $RedirectStandardError,
        [switch] $RedirectStandardOutput,
        [parameter(ParameterSetName="Passthru")] [switch] $Passthru,
        [parameter(ParameterSetName="CheckExitCode")] [switch] $CheckExitCode
    )
    write-verbose "Running Invoke-ProcessAndWait in verbose mode. WARNING: this may show sensitive commandline arguments (passwords, connection strings) and should only be used in development!"
    write-verbose "Running '$command' with arguments '$argumentList'"
    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = New-Object System.Diagnostics.ProcessStartInfo
    $process.StartInfo.FileName = $command

    #$process.StartInfo.RedirectStandardError = $RedirectStandardError
    #if ($ShowStandardOutput) {
    if ($RedirectStandardOutput) {
        $process.StartInfo.RedirectStandardOutput = $true
    }
    if ($RedirectStandardError) {
        $process.StartInfo.RedirectStandardError = $true
    }
    $process.StartInfo.UseShellExecute = $false # AKA don't run in a new window
    $process.StartInfo.Arguments = $argumentList
    $process.Start() | Out-Null
<#
    if ($RedirectStandardOutput) {
        $line = $process.StandardOutput.ReadLine()
        while ($line -ne $null) {
            Write-Host $line
            $line = $process.StandardOutput.ReadLine()
        }
    }
#>
    $process.WaitForExit()
    write-verbose "Process exited with exit code $($process.ExitCode)"
    if ($PSCmdlet.ParameterSetName -eq "CheckExitCode") {
        if ($process.ExitCode -ne 0) {
            write-verbose "Command $command with arguments '$argumentList' exited with code $($process.ExitCode)"
            throw "Command '$command' with $($argumentList.count) arguments exited with code $($process.ExitCode)"
        }
    }
    else {
        return $process
    }
}


function Generate-Password {
    param([int]$length=8)
    # From: http://ronalddameron.blogspot.com/2009/09/two-lines-of-powershell-random.html
    $null = [Reflection.Assembly]::LoadWithPartialName("System.Web")
    [System.Web.Security.Membership]::GeneratePassword($length,2)  # 8 bytes long
}
set-alias pwgen generate-password

function ConvertTo-Base64($string) {
   $bytes  = [System.Text.Encoding]::UTF8.GetBytes($string)
   [System.Convert]::ToBase64String($bytes)
}
function ConvertFrom-Base64($string) {
   $bytes  = [System.Convert]::FromBase64String($string)
   [System.Text.Encoding]::UTF8.GetString($bytes)
}

function llm {
    get-childitem $args | sort-object -property lastwritetime
}

function .. { cd .. }
function ... { cd ../.. }
function .... { cd ../../.. }


if (test-path alias:more) { del alias:more }
if (test-path function:more) { del function:more }
if (test-path alias:l) { del alias:l }
set-alias l less

function vless {
    # Adapted from vim/macros/less.bat. Assumes vim is in path though.
    [cmdletbinding()]
    param(
        [Parameter(Mandatory=$True,ValueFromPipeline=$True)] [string] $filename
    )
    if ($input) {
        $input | vim --cmd "let no_plugin_maps = 1" -c "runtime! macros/less.vim" -
    }
    else {
        vim --cmd "let no_plugin_maps = 1" -c "runtime! macros/less.vim" $filename
    }
}
set-alias vl vless

function Get-GitPrettyLog {
    git log --oneline --all --graph --decorate $args
}
set-alias gpl Get-GitPrettyLog
function Compare-GitObjectsOnGitHub {
    param(
        [parameter(mandatory=$true)] [string] $CommitA,
        [parameter(mandatory=$true)] [string] $CommitB,
        [string] $remoteName = "origin"
    )
    $remotes = git remote -v
    foreach ($r in $remotes) {
        $name,$address,$direction = $r -split "\s+"
        if (($name -eq $remoteName) -and ($direction -eq "(fetch)")) {
            $remote = $address -replace "^git\@github\.com:","" -replace "\.git$",""
            break
        }
    }
    if (-not $remote) {
        throw "Couldn't find remote, probably because this is a dumb hack???? Yah."
    }
    start "https://github.com/$remote/compare/$CommitA...$CommitB"
}
set-alias github-diff Compare-GitObjectsOnGitHub

function Get-MagicNumber {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)] [String[]] $filePath,
        [Int] $bytes = 2
    )
    process {
        $firstBytes = Get-Content $filePath -Encoding Byte | Select-Object -First $bytes
        New-Object PSObject -Property @{
            FullName = Get-Item $filePath | Select-Object -Expand FullName
            FirstBytesDecimal = $firstBytes
            FirstBytesHexadecimal = $firstBytes |% { "0x{0:X0}" -f $_ }
        }
    }
}

function Get-Profiles {
    [cmdletbinding()]
    param(
        [string] $containingPattern,
        [switch] $caseSensitive
    )
    $profiles = gci -recurse $MrledProfile.DHDProfile,$MrledProfile.DHDOpt -include *.ps1,*.psm1
    @($Profile.AllUsersAllHosts,$Profile.AllUsersCurrentHost,$Profile.CurrentUserAllHosts,$Profile.CurrentUserCurrentHost) |% { if (Test-Path $_) {$profiles += @($_)} }

    if ($containingPattern) {
        write-verbose "Searching through results files for strings matching '$containingPattern'..."
        $profiles |? { $_ | sls -quiet -pattern $containingPattern -caseSensitive:$caseSensitive }
    }
    else {
        $profiles
    }
}
set-alias gpro Get-Profiles

# Make output of get-command better (more like Unix) for interactive use.
# NOTE: For aliases, the processing function calls the show function again - this is recursive!
# it's so if you have an alias chain like x->y->z->, where x and y are aliases
# and z is a function, you'll get the whole relationship + the function definition as well.
function Display-AllCommands {
    param(
        [alias("r","a","all")] [switch]$recurse,
        [int]$recursionlevel=0,
        # weird syntax means that if the $recursionlevel isn't specified,
        # $args[0] doesn't become $recursionlevel:
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args
    )
    if ($args.Count -le 0) {return}
    foreach ($a in $args) {
        $level = $recursionlevel
        # This next line helps keep track if there is lots of output, but also clutters everything. Hmm.
        #if ($level -eq 0) {write-host ($a) -foregroundcolor Green}
        if ($level -gt 20) {
            $errstr  = "Recursion is greater than 20 levels deep. Probably a circular set of aliases? "
            write-error $errstr
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

                        $regex = [system.text.regularexpressions.regex]
                        $reml = [System.Text.RegularExpressions.RegexOptions]::MultiLine
                        $re_firstnewline = new-object $regex ('\A\r?\n', $reml)
                        $re_lastnewline = new-object $regex ('\Z\r?\n', $reml)
                        $re_newline = new-object $regex ('\r?\n', $reml)
                        $re_stringbegin = new-object $regex ('\A', $reml)

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
# function Setup-TestForWh {
#     set-alias ttt___ttt uuu___uuu
#     set-alias uuu___uuu vvv___vvv
#     set-alias vvv___vvv WSManHTTPConfig #an exe file in system32 on x64 win7
#     set-alias WSManHTTPConfig xxx___xxx
#     set-alias xxx___xxx get-authenticodesignature #existing cmdlet
#     set-alias get-authenticodesignature zzz___zzz
#     set-alias zzz___zzz create-shortcut
#     function ttt___ttt { echo "functiontest" }
#     function xxx___xxx { echo "functiontest" }
#     function ttt___ttt { echo "functiontest" }
#     function ttt___ttt { echo "functiontest" }
#     function WSManHTTPConfig { echo "hurr's a function"; cmd.exe }
#     function get-authenticodesignature { echo "functest"; get-content C:\boot.ini; echo "ZUHH" }
#     set-alias aaa___aaa bbb___bbb #recursive
#     set-alias bbb___bbb aaa___aaa #recursive
# }


# by defaul, touch is aliased to set-filetime, which doesn't create new empty files.
if (test-path alias:touch) {del alias:touch}
function touch {
    param([parameter(mandatory=$true)] [string[]] $file)
    foreach ($f in $file) {
        if (test-path $f) {
            set-filetime $f
        }
        else {
            new-item -ItemType file $f
        }
    }
}

if (test-path alias:man) { del alias:man }
function man {
    foreach ($a in $args) {
        get-help $a -full | less.bat
    }
}
<#
.synopsis
Get the syntax for a command
.description
If you do (Get-Help Something).Syntax, it will return just the syntax for the command. Yay.
... Unless it's a function without a documentation block. Then it returns an ugly SyntaxItem object.
It's mostly the same thing, but if a PSObject is of type `MamlCommandHelpInfo#syntax`, then it
displays is properly. All this does is check to see if the .Syntax object you get from Get-Help
contains that type; if it doesn't, it adds it before returning it.
#>
function Get-Syntax {
    param(
        [string[]] $command
    )
    foreach ($cmd in $command) {
        $cmdSyntax = (get-help $cmd).Syntax
        if (-not $cmdSyntax.PSObject.TypeNames.Contains("MamlCommandHelpInfo#syntax")) {
            $cmdSyntax.PSObject.TypeNames.Insert(0,"MamlCommandHelpInfo#syntax")
        }
        $cmdSyntax
    }
}
set-alias syntax get-syntax

function Set-LocationMrl {
    param(
        [parameter(position=0, valuefrompipeline=$true)] $location = $home
    )
    Set-Location $location
}
if (Test-Path Alias:\cd) { Remove-Item Alias:\cd }
Set-Alias cd Set-LocationMrl -Force

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

function New-MRLShortcut {
    param(
        [parameter(Mandatory=$true)] [string] $linkPath,
        [parameter(Mandatory=$true)] [string] $targetPath,
        [string] [ValidateSet("Activate","Maximize","Minimize")] $windowStyle = "Activate",
        [string] $arguments,
        [switch] $force,
        [switch] $PassThru
    )
    if (-not [System.IO.Path]::IsPathRooted($linkPath)) {
        $linkPath = "$pwd\$linkPath"
    }
    if (-not $linkPath.tolower().endswith(".lnk")) {
        # required, or you'll get an error message and fail.
        $linkPath = "$linkPath.lnk"
    }
    if ((test-path $linkPath) -and (-not $force.ispresent)) {
        # I don't think I care to check if there's a non-link file named .lnk that we're going to overwrite
        write-error ("linkPath $linkPath already exists; use -force to overwrite.")
        return $null
    }
    $wshshell = New-Object -ComObject WScript.Shell
    $lnk = $wshshell.CreateShortcut($linkPath)

    switch ($windowStyle) {
        "Activate" { $lnk.WindowStyle = 1 }
        "Maximize" { $lnk.WindowStyle = 2 }
        "Minimize" { $lnk.WindowStyle = 7 }
    }

    $lnk.targetPath = "$targetPath"
    $lnk.Arguments = "$arguments" #it's ok if this is $null
    $lnk.save()
    if ($PassThru) {
        return $lnk
    }
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

# todo : could mimic set-variable options as closely aspossible
function Set-WinEnvironmentVariable {
    [CmdletBinding()] param(
        [parameter(mandatory=$true)] [string] $Name,
        [string] $Value = "",
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = "Process"
    )
    foreach ($target in $TargetLocation) {
        Write-Verbose "Setting '$Name' = '$Value' in '$target' scope"
        [Environment]::SetEnvironmentVariable($Name, $Value, $target)
    }
}
set-alias setenv Set-WinEnvironmentVariable
# TODO: when getting multiple targets, it outputs an array of strings for the value
#       honestly not sure what to do here. %PATH% is *concatenated*, but others are
#       *replaced* when there's a user and a machine one. ?????
function Get-WinEnvironmentVariable {
    param(
        [parameter(mandatory=$true)] [string] $Name,
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = @("Machine","User","Process")
    )
    $out = ""
    foreach ($target in $TargetLocation) {
        ([Environment]::GetEnvironmentVariable($Name, $target))
    }
}
set-alias getenv Get-WinEnvironmentVariable

# http://social.msdn.microsoft.com/Forums/vstudio/en-US/630ed1d9-73f1-4cc0-bc84-04f29cffc13b/
function Set-FileAssociation {
    param(
        [parameter(mandatory=$true)] [string] $extension,
        [parameter(mandatory=$true)] [string] $association,
        [string] $contentType,
        [validateset("User","Machine")] $location = "User"
    )
    if ($location.tolower() -eq "user") {
        $drive = "HKCU"
    }
    elseif ($location.tolower() -eq "machine") {
        if (-not (Test-AdminRole)) {
            write-error "Cannot set associations for the whole machine unless running as administrator."
            return
        }
        $drive = "HKLM"
    }
    $key = "$($drive):\Software\Classes\$extension"
    if (-not (test-path $key)) {
        new-item -force $key | out-null
    }
    set-itemproperty $key "(default)" $association
    set-itemproperty $key "Content Type" $contentType
}
function Set-AssociationOpenCommand {
    param(
        [parameter(mandatory=$true)] [string] $association,
        [parameter(mandatory=$true)] [string] $command,
        [validateset("User","Machine")] $location = "User"
    )
    if ($location.tolower() -eq "user") {
        $drive = "HKCU"
    }
    elseif ($location.tolower() -eq "machine") {
        if (-not (Test-AdminRole)) {
            write-error "Cannot set associations for the whole machine unless running as administrator."
            return
        }
        $drive = "HKLM"
    }
    $key = "$($drive):\Software\Classes\$association\shell\open\command"
    if (-not (test-path $key)) {
        new-item -force $key | out-null
    }
    set-itemproperty $key "(default)" $command
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
    $comment = $pcontent[1].split("`"")[1]
    $newcontent += " $comment"
    return $newcontent
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
    $secureSshPass = read-host -AsSecureString "Password for $hostname"
    $sshPass = Decrypt-SecureString $secureSshPass
    "",$keydata | plink -pw "$sshPass" $hostname "mkdir -p ~/.ssh && cat - >> $akeys && chmod 700 ~/.ssh && chmod 600 $akeys"
}


$bvssh = "${env:ProgramFiles(x86)}\Bitvise SSH Client"
if (test-path $bvssh) {

    function Invoke-BitviseSsh {
        param(
            $bvExe = "stermc.exe",
            [parameter(Position=0, ValueFromRemainingArguments=$true)] $args
        )
        $bvArgs = $args
        $bvArgs+= @("-keypairFile=$Home\.ssh\id_rsa")
        $bvArgs+= @("-hostKeyFile=$Home\.dhd\hbase\known_hosts")
        start-process -wait -nonewwindow $bvssh\$bvExe -argumentList $bvArgs
    }

    function Invoke-BitviseSshScreenSession {
        param(
            [parameter(mandatory=$true)] [alias('r')] [string] $hostname,
            [string] $screenSession = "camelot"
        )
        Invoke-BitviseSsh $hostname '-cmd=scr'
    }
    set-alias scr Invoke-BitviseSshScreenSession

    function Start-SSHTunnel {
        param(
            [string] $serverName = "mrled@willow.younix.us"
        )
        Invoke-BitviseSsh -bvExe stnlc.exe -proxyFwding=y -proxyListPort=2001 $serverName
    }

    function Get-BitviseKnownHosts {
        [cmdletbinding()]
        param(
            [string] $hostname
        )
        $HostKeysReg = get-item hkcu:\Software\Bitvise\HostKeys
        $HostKeys = @{}
        foreach ($entry in ($HostKeysReg.GetValueNames() |where {$_.StartsWith("HostKey2_")} )) {
            write-debug $entry

            $fingerprint = ($entry -split '_')[3][0..31] -join ""
            write-verbose "Fingerprint: $fingerprint"

            $fullValue = $HostKeysReg.GetValue($entry)
            $relevantValue = $fullValue[6..($fullValue.length -1)]

            $postHostnamePattern = 0,0,0,22

            $foundIt = $false
            for ($i = 0; $i -lt $relevantValue.Length; $i += 1) {
                for ($j = 0; $j -lt $postHostnamePattern.length; $j += 1) {
                    if (-not ($relevantValue[$i + $j] -eq $postHostnamePattern[$j])) {
                        $foundIt = $false
                        break
                    }
                    $foundIt = $true
                }
                if ($foundIt) {
                    $postHostnameIndex = $i
                    break
                }
            }
            if (-not $postHostnameIndex) {
                throw "Failed to find the end of the hostname after searching through $i positions"
            }
            $binHostname = $relevantValue[0..($postHostnameIndex - 1)]
            write-verbose ($binHostname -join ",")
            $asciiHostname = (New-Object System.Text.ASCIIEncoding).GetString($binHostname)
            write-verbose "Hostname: $asciiHostname"
            $HostKeys.$asciiHostname = $fingerprint
        }

        if ($hostname -and $hostKeys.$hostname) {
            return @{ $hostname = $hostKeys.$hostname }
        }
        elseif ($hostname) {
            throw "No host key for hostname $hostname"
        }
        else {
            return $hostKeys
        }
    }
}

# http://pastie.org/2867807
function Generate-SparkLine {
    #$ticks = @(' ', [char]9601, [char]9602, [char]9603, [char]9604, [char]9605, [char]9606, [char]9607, [char]9608)
    $ticks = @([char]9601, [char]9602, [char]9603, [char]9604, [char]9605, [char]9606, [char]9607, [char]9608)

    $minmax = $args | measure -min -max
    $range = $minmax.Maximum - $minmax.Minimum
    $scale = $ticks.length - 1
    $output = @()

    foreach ($x in $args) {
       $output += $ticks[([Math]::round((($x - $minmax.Minimum) / $range) * $scale))]
    }

    return [String]::join('', $output)
}
set-alias spark Generate-SparkLine

set-alias getmo Get-Module
set-alias rmmo Remove-Module

# from: http://andyarismendi.blogspot.com/2013/04/out-clipboard-cmdlet.html
function Set-Clipboard {
    [cmdletbinding()]
    param (
        [parameter(Position=0,Mandatory=$true,ValueFromPipeline=$true)]$InputObject,
        [switch] $File
    )
    begin {
        # STA is required to set the clipboard. (...whatever)
        # Creating a new runspace is much faster than a whole new Powershell process
        $ps = [PowerShell]::Create()
        $rs = [RunSpaceFactory]::CreateRunspace()
        $rs.ApartmentState = "STA"
        $rs.ThreadOptions = "ReuseThread"
        $rs.Open()
        $data = @()
    }
    process {$data += $InputObject}
    end {
        $rs.SessionStateProxy.SetVariable("do_file_copy", $File)
        $rs.SessionStateProxy.SetVariable("data", $data)
        $ps.Runspace = $rs
        $ps.AddScript({
            Add-Type -AssemblyName 'System.Windows.Forms'
            if ($do_file_copy) {
                $file_list = New-Object -TypeName System.Collections.Specialized.StringCollection
                $data | % {
                    if ($_ -is [System.IO.FileInfo]) {[void]$file_list.Add($_.FullName)}
                    elseif ([IO.File]::Exists($_))    {[void]$file_list.Add($_)}
                }
                [System.Windows.Forms.Clipboard]::SetFileDropList($file_list)
            } else {
                $host_out = (($data | Out-String -Width 1000) -split "`n" | % {$_.TrimEnd()}) -join "`n"
                [System.Windows.Forms.Clipboard]::SetText($host_out)
            }
        }).Invoke()
    }
}
set-alias Out-Clipboard Set-Clipboard

function Test-PowershellSyntax {
    [cmdletbinding(DefaultParameterSetName='FromFile')]
    param(
        [parameter(mandatory=$true, Position=0, ValueFromPipeline=$true, ParameterSetName='FromText')] [string] $text,
        [parameter(mandatory=$true, Position=0, ParameterSetName='FromFile')] [string] $fileName,
        [switch] $ThrowOnFailure
    )
    $tokens = @()
    $parseErrors = @()
    $parser = [System.Management.Automation.Language.Parser]
    if ($pscmdlet.ParameterSetName -eq 'FromText') {
        $parsed = $parser::ParseInput($text, [ref]$tokens, [ref]$parseErrors)
    }
    elseif ($pscmdlet.ParameterSetName -eq 'FromFile') {
        $fileName = resolve-path $fileName
        $parsed = $parser::ParseFile($fileName, [ref]$tokens, [ref]$parseErrors)
    }
    write-verbose "$($tokens.count) tokens found."

    if ($parseErrors.count -gt 0) {
        $message = "$($parseErrors.count) parse errors found in file '$fileName':`r`n"
        $parseErrors |% { $message += "`r`n    $_" }
        if ($ThrowOnFailure) { throw $message } else { write-verbose $message }
        return $false
    }
    return $true
}

function Decrypt-SecureString {
    param(
        [Parameter(ValueFromPipeline=$true,Mandatory=$true,Position=0)] [System.Security.SecureString] $secureString
    )
    $marshal = [System.Runtime.InteropServices.Marshal]
    $pointer = $marshal::SecureStringToBSTR($secureString)
    $decryptedString = $marshal::PtrToStringBSTR($pointer)
    $marshal::ZeroFreeBSTR($pointer)
    return $decryptedString
}

function Show-ErrorReport {
    [cmdletbinding()]
    param(
        [switch] $ExitIfErrors,
        [Array] $errorList = $Error,
        [Int] $exitCode = $LASTEXITCODE
    )

    function WrapText {
        param($text, $width, $indentSpaces)
        $width = $width -1
        $indent = " " * $indentSpaces
        foreach ($line in ($text -split "`n")) {
            while ($line.length -gt $width) {
                $line = "$indent$line"
                Write-Output $line.substring(0,$width)
                $line = $line.substring($width)
            }
            Write-Output "$indent$line"
        }
    }

    $wrapWidth = if ($Host -and $Host.UI -and $Host.UI.RawUI -and ($Host.UI.RawUI.Buffersize.Width -gt 0)) {$Host.UI.RawUI.Buffersize.Width} else {9999}
    if ($errorList.count -or $exitCode) {
        Write-Output "ERROR Report: `$LASTEXITCODE=$exitCode, `$Error.count=$($Error.count)"
        for ($i=$errorList.count -1; $i -ge 0; $i-=1) {
            $err = $errorList[$i]
            Write-Output "`$Error[$i]:"

            # $error can contain at least 2 kind of objects - ErrorRecord objects, and things that wrap ErrorRecord objects
            # The information we need is found in the ErrorRecord objects, so unwrap them here if necessary
            if ($err.PSObject.Properties['ErrorRecord']) {$err = $err.ErrorRecord}

            WrapText -text $err.ToString() -width $wrapWidth -indentSpaces 4

            if ($err.ScriptStackTrace) {
                WrapText -text $err.ScriptStackTrace -width $wrapWidth -indentSpaces 8
            }
        }
        if ($ExitIfErrors) {
            exit 1
        }
    }
    else {
        Write-Output "ERROR Report: No errors"
    }
}
set-alias err Show-ErrorReport

function Clear-Error {
    $error.clear()
    $global:LASTEXITCODE = 0
}
set-alias clerr Clear-Error

function Get-ErrorType {
    [CmdletBinding()] Param(
        $errorObject = $Error[0]
    )
    if (-not $errorObject) {
        throw "No object passed as -errorObject, and `$Error is empty"
    }

    $exception = if ($errorObject.Exception) {$errorObject.Exception} else {$errorObject}
    $errNamespace = $exception.GetType().Namespace
    $errName = $exception.GetType().Name
    return "${errNamespace}.${errName}"
}

function Format-XML {
    Param (
        [Parameter(ValueFromPipeline=$true,Mandatory=$true,Position=0)] [System.Array] $xml
    )
    foreach ($xmlItem in $xml) {
        $StringWriter = New-Object system.io.stringwriter
        $XmlWriter = New-Object system.xml.xmltextwriter($StringWriter)
        $XmlWriter.Formatting = [System.xml.formatting]::Indented
        $xmlItem.WriteContentTo($XmlWriter)
        $StringWriter.ToString()
    }
}

set-alias PrettyPrint-XML Format-XML

<#
.SYNOPSIS
Creates an XML-based representation of an object or objects and outputs it as a string.

.DESCRIPTION
The Out-Clixml cmdlet creates an XML-based representation of an object or objects and outputs it as a string. This cmdlet is similar to Export-CliXml except it doesn't output to a file.
From: http://poshcode.org/3770

.PARAMETER Depth
Specifies how many levels of contained objects are included in the XML representation. The default value is 2 because the default value of Export-CliXml is 2.

.PARAMETER InputObject
Specifies the object to be converted. Enter a variable that contains the objects, or type a command or expression that gets the objects. You can also pipe objects to Out-Clixml.

.INPUTS
System.Management.Automation.PSObject

You can pipe any object to Out-CliXml.
#>
function Out-CliXml {
    [CmdletBinding()] Param (
        [Parameter(ValueFromPipeline = $True, Mandatory = $True)] [PSObject] $InputObject,
        [ValidateRange(1, [Int32]::MaxValue)] [Int32] $Depth = 2
    )
    [System.Management.Automation.PSSerializer]::Serialize($InputObject, $Depth)
}

function In-CliXml {
    [CmdletBinding()] Param (
        [Parameter(ValueFromPipeline = $True, Mandatory = $True)] [String] $InputXml
    )
    [System.Management.Automation.PSSerializer]::Deserialize($InputXml)
}

<#
.synopsis
Start a batch file
.description
Start a batch file, and prevent a stupid "Terminate batch job? Y/N" prompt if
you Ctrl-C the process.
#>
function Start-BatchFile {
    [CmdletBinding()] Param(
        [parameter(mandatory=$true)] [string] $batchFile,
        [parameter(ValueFromRemainingArguments=$true)] $batchArgs
    )
    # we use "<NUL" to prevent that fucking "Terminate batch job? Y/N" prompt
    cmd.exe "/c $batchFile $batchArgs <NUL"
}
set-alias bat Start-BatchFile


<#
.synopsis
Fucking extract an archive the right way.
.description
Fucking extract an archive the right way:
- Create a new temporary directory inside $outDir
- Use 7z.exe to extract the archive to that temp dir
  - If the only item in the archive is a .tar file, unarchive that file as well
- Make sure exactly one file ends up in $outDir:
  - If there was only one item in the archive (after extracting the .tar file,
    if applicable), move it to $outDir
  - If there was more than one item in the archive, rename the temp dir to
    something sensible based on the archive name. (For example, if the archive
    name is SomeArchive.zip, rename the temp dir to SomeArchive)
.parameter archive
A list of archives to extract
.parameter outDir
The directory to extract the archives to. Defaults to the current working
directory.
.parameter force
If there is an existing file/directory with the same name as one that would be
extracted, delete the existing item first.
#>
function Extract-FuckingArchive {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string[]] $archive,
        [string] $outDir,
        [switch] $force
    )

    <#
    .synopsis
    Fucking extract an archive to a temporary directory
    .parameter archive
    An archive file
    .parameter outDir
    The directory in which to create the temporary extraction dir
    .parameter noOutDir
    Instead of creating a temporary extraction dir, just use the archive's parent directory
    #>
    function fuckingExtractOneLayer {
        [cmdletbinding()] param(
            [parameter(mandatory=$true)] [System.IO.FileInfo] $archive,
            [parameter(mandatory=$true,parametersetname="outDir")] [System.IO.DirectoryInfo] $outDir,
            [parameter(mandatory=$true,parametersetname="noOutDir")] [switch] $noOutDir
        )

        if ($noOutDir) {
            $outDir = $archive.directory.fullname
        }
        else {
            $tempDirName = "fuckingextract-" + [System.IO.Path]::GetRandomFileName()
            $outDir = "$($outDir.fullname)\$tempDirName"
            if (test-path $outDir) {
                throw "The temporary directory that already exists"
            }
            mkdir $outDir | out-null
        }

        $7zcmd = '7z x "-o{0}" "{1}"' -f @($outDir, $archive.fullName)
        $7zout = iex $7zcmd
        if ($LASTEXITCODE -ne 0) {
            throw "7z exited with code $LASTEXITCODE`n`tcommand line: $7zcmd`n`toutput: `n$7zout"
        }
        return $outDir
    }

    try {
        gcm 7z | out-null
    }
    catch {
        throw "7z.exe is not in your `$ENV:PATH; cannot continue"
    }

    $secondLayerExtensions = @(".tar") # There aren't any more that I can think of?

    if (-not $outDir) {
        $outDir = $pwd
    }
    elseif (-not (test-path $outDir)) {
        mkdir -force $outDir | out-null
    }
    $outDirItem = get-item $outDir

    $outFiles = @()
    foreach ($arch in $archive) {
        $archItem = get-item $arch

        # this is the name of the archive w/o its extension
        # this will be used as the eventual directory name to extract to
        $archBareName = [System.IO.Path]::GetFileNameWithoutExtension($archItem.name)

        $exDir = fuckingExtractOneLayer -archive $archItem -outdir $outDirItem
        write-verbose "Using temporary extraction directory: $exDir"
        $exItems = gci $exDir

        # If there is only one item in the archive, AND that item has an
        # extension in $secondLayerExtensions, extract that item too.
        if (((gci $exDir).count -eq 1) -and
            ($secondLayerExtensions |? { $exItems[0].name.endswith($_) }) )
        {
            $innerArchItem = $exItems[0]
            write-verbose "Found inner archive: $($innerArchItem.fullname)"
            fuckingExtractOneLayer -archive $innerArchItem.fullname -noOutDir | out-null
            $archBareName = [System.IO.Path]::GetFileNameWithoutExtension($innerArchItem.name)
            rm $innerArchItem.fullname
            $exItems = gci $exDir
        }

        # If there is only one item in the archive, we don't need the
        # extraction directory - just move the item into the output dir
        if ($exItems.count -eq 1) {
            $outItem = $exItems[0]
            $outItemName = "$($outDirItem.fullname)\$($outItem.name)"
            write-verbose "Only one item in archive: '$($outItem.fullname)'; moving to '$($outDirItem.fullname)'"

            if ((test-path $outItemName) -and $force) {
                write-verbose "Found existing item at '$outItemName' but -force was specified; removing..."
                rm -recurse -force $outItemName
            }
            elseif ((test-path $outItemName) -and -not $force) {
                throw "Extracted archive to '$exDir' but could not move to '$outItemName' because '$outItemName' already exists"
            }

            $outFiles += @( mv $outItem.fullname $outItemName -passthru )
            rm -recurse $exDir
        }
        # If there's more than item in the archive, then rename the dir to the
        # bare name of the archive
        else {
            $outItemName = "$($outDirItem.fullName)\$archBareName"
            write-verbose "Multiple items in archive; moving temporary extraction directory to '$outItemName'"

            if ((test-path $outItemName) -and $force) {
                write-verbose "Found existing item at '$outItemName' but -force was specified; removing..."
                rm -recurse -force $outItemName
            }
            elseif ((test-path $outItemName) -and -not $force) {
                throw "Extracted archive to '$exDir' but could not move to '$outItemName' because '$outItemName' already exists"
            }

            $outFiles += @( mv $exDir $outItemName -passthru )
        }
    }

    return $outFiles
}

set-alias Fucking-Extract Extract-FuckingArchive
set-alias fex Extract-FuckingArchive

<#
.synopsis
Send data over the network

.description
Send data over the network
Sort of like you might wanna do with netcat/nc, but too different to be "nc for powershell"

Source: https://gist.github.com/jstangroome/9adaa87a845e5be906c8

.example 'GET / HTTP/1.0', '' | Send-NetworkData -Computer www.powershellmagazine.com -Port 80
Pipe in a HTTP request

.example Send-NetworkData -Data 'GET / HTTP/1.0', '' -Computer www.powershellmagazine.com -Port 80 -Timeout 0:00:02
Use the Data parameter to do the same but only wait 2 seconds for a response:

.example Send-NetworkData -Data "EHLO $Env:ComputerName", "QUIT" -Computer mail.example.com -Port 25
Say hello to an SMTP server
#>
function Send-NetworkData {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory)] [string] $Computer,
        [Parameter(Mandatory)] [ValidateRange(1, 65535)] [Int16] $Port,
        [Parameter(ValueFromPipeline)] [string[]] $Data,
        [System.Text.Encoding] $Encoding = [System.Text.Encoding]::ASCII,
        [TimeSpan] $Timeout = [System.Threading.Timeout]::InfiniteTimeSpan
    )
    begin {
        # establish the connection and a stream writer
        $Client = New-Object -TypeName System.Net.Sockets.TcpClient
        $Client.Connect($Computer, $Port)
        $Stream = $Client.GetStream()
        $Writer = New-Object -Type System.IO.StreamWriter -ArgumentList $Stream, $Encoding, $Client.SendBufferSize, $true
    }
    process {
        # send all the input data
        foreach ($Line in $Data) {
            $Writer.WriteLine($Line)
        }
    }
    end {
        # flush and close the connection send
        $Writer.Flush()
        $Writer.Dispose()
        $Client.Client.Shutdown('Send')

        # read the response
        $Stream.ReadTimeout = [System.Threading.Timeout]::Infinite
        if ($Timeout -ne [System.Threading.Timeout]::InfiniteTimeSpan) {
            $Stream.ReadTimeout = $Timeout.TotalMilliseconds
        }

        $Result = ''
        $Buffer = New-Object -TypeName System.Byte[] -ArgumentList $Client.ReceiveBufferSize
        do {
            try {
                $ByteCount = $Stream.Read($Buffer, 0, $Buffer.Length)
            }
            catch [System.IO.IOException] {
                $ByteCount = 0
            }
            if ($ByteCount -gt 0) {
                $Result += $Encoding.GetString($Buffer, 0, $ByteCount)
            }
        } while ($Stream.DataAvailable -or $Client.Client.Connected)
        Write-Output $Result
        # cleanup
        $Stream.Dispose()
        $Client.Dispose()
    }
}

<#
.Description
Retrieves all available Exceptions in the current session. Returns an array of strings which represent the exception type names.
.Notes
Originally from: http://www.powershellmagazine.com/2011/09/14/custom-errors/
#>
function Get-AvailableExceptionsList {
    [CmdletBinding()]
    param()
    $irregulars = 'Dispose|OperationAborted|Unhandled|ThreadAbort|ThreadStart|TypeInitialization'
    $RelevantExceptions = @()
    foreach ($assembly in [AppDomain]::CurrentDomain.GetAssemblies()) {
        try {
            $AllExceptions = $assembly.GetExportedTypes() -match 'Exception' -notmatch $irregulars
        }
        catch {
            write-verbose "Could not get exported types for assembly '$assembly'"
            continue
        }
        foreach ($exc in $AllExceptions) {
            if (-not $exc.GetConstructors()) {
                write-verbose "No constructors for '$exc' in '$assembly'"
                continue
            }
            try {
                $TestException = New-Object $exc.FullName
                $TestError = New-Object Management.Automation.ErrorRecord $TestException,ErrorID,OpenError,Target
            }
            catch { # Tests failed, don't add this as a relevant exception
                write-verbose "Could not create test exception/error for '$exc' in '$assembly'"
                continue
            }
            $RelevantExceptions += $exc.FullName
        }
    }
    return $RelevantExceptions
}

<#
.description
Return the name of the service account a particular service runs under
.notes
See the LsaSecrets module for information on stealing the service account password from the LSA secrets store
#>
function Get-ServiceAccountName {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] $serviceName
    )
    $wmiObj = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"
    $service = Get-Service $serviceName
    New-Object PSObject -Property @{
        ServiceName = $service.Name
        DisplayName = $service.DisplayName
        ServiceAccount = $wmiObj.StartName
    }
}

<#
.ForwardHelpTargetName Microsoft.PowerShell.Core\Enter-PSSession
.ForwardHelpCategory Cmdlet
#>
function remote {
    [CmdletBinding(HelpUri='http://go.microsoft.com/fwlink/?LinkID=135210', RemotingCapability='OwnedByCommand')]
    param(
        [Parameter(Mandatory=$true)] [Alias('Cn')] [ValidateNotNullOrEmpty()] [string] ${ComputerName},
        [switch] ${EnableNetworkAccess},
        [pscredential] [System.Management.Automation.CredentialAttribute()] ${Credential},
        [ValidateRange(1, 65535)] [int] ${Port},
        [switch] ${UseSSL},
        [string] ${ConfigurationName},
        [string] ${ApplicationName},
        [ValidateNotNull()] [System.Management.Automation.Remoting.PSSessionOption] ${SessionOption},
        [System.Management.Automation.Runspaces.AuthenticationMechanism] ${Authentication},
        [string] ${CertificateThumbprint}
    )
    begin {
        try {
            $outBuffer = $null
            if ($PSBoundParameters.TryGetValue('OutBuffer', [ref]$outBuffer)) {
                $PSBoundParameters['OutBuffer'] = 1
            }

            # We're going to get the remote session and load our local profile into it
            $npssParams = @{}
            $npssParams['ComputerName'] = $PSBoundParameters.ComputerName
            foreach ($remoteParam in @('ApplicationName', 'Authentication', 'CertificateThumbprint', 'ConfigurationName', 'Credential', 'EnableNetworkAccess', 'Port', 'Sessionoption')) {
                if ($PSBoundParameters[$remoteParam]) {
                    $npssParams[$remoteParam] = $PSBoundParameters[$remoteParam]
                }
            }
            $session = New-PSSession @npssParams
            $PSBoundParameters['Session'] = $session

            $wrappedCmd = $ExecutionContext.InvokeCommand.GetCommand('Microsoft.PowerShell.Core\Enter-PSSession', [System.Management.Automation.CommandTypes]::Cmdlet)
            $scriptCmd = {& $wrappedCmd @PSBoundParameters }
            $steppablePipeline = $scriptCmd.GetSteppablePipeline($myInvocation.CommandOrigin)
            $steppablePipeline.Begin($PSCmdlet)
        } catch {
            throw
        }
    }
    process {
        try {
            $steppablePipeline.Process($_)
        } catch {
            throw
        }
    }
    end {
        try {
            $steppablePipeline.End()
        } catch {
            throw
        }
    }
}

function Set-ConEmuTabTitle {
    [cmdletbinding()] param(
        $title
    )
    if ($title) { $title = ":$title" }
    $fullTitle = "$($SpecialCharacters.DoublePrompt)$title" -replace " ",''
    $macro = "Rename(0,$fullTitle"
    Write-Verbose "Running macro: $macro"
    $out = conemuc /guimacro $macro
    if ($out -ne "OK") {
        throw "Failed to change tab title with error: $out"
    }
}
Set-Alias Rename-Tab Set-ConEmuTabTitle

# Edge bullshit
# Based on information from: https://gyorgybalassy.wordpress.com/2015/12/21/favorites-and-bookmarklets-in-microsoft-edge/

function Get-EdgeBrowserBookmark {
    [CmdletBinding()] Param()
    $regKey = 'HKCU:\SOFTWARE\Classes\Local Settings\Software\Microsoft\Windows\CurrentVersion\AppContainer\Storage\microsoft.microsoftedge_8wekyb3d8bbwe\MicrosoftEdge\FavOrder\FavBarCache'
    $regItems = Get-Item "$regKey\*"
    foreach ($regItem in $regItems) {

    }
}

function Set-EdgeBrowserBookmark {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$True)] [String] $name,
        [Parameter(Mandatory=$True)] [String] $newUrl
    )
    $bkmk = Get-EdgeBrowserBookmark |? -Property Name -eq $name
    # if ($bkmk.)
}
