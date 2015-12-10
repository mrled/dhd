# [char]955     λ (GREEK LETTER LAMBDA)
# [char]9773    ☭ (HAMMER AND SICKLE)
# [char]42479   ꗯ (VAI SYLLABLE GBE)
# [char]1003    ϫ (COPTIC SMALL LETTER GANGIA)
# [char]187     » (RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK)
# [char]7       beeps @ u
$LambdaChar = "$([char]955)"
$DoublePromptChar = [char]187
$HammerAndSickleChar = "$([char]9773)"
$VisualStudioChar = "$([char]42479)"
$BeepChar = @([char]7)


$ExternalBinaryPathSearchPatterns = @{
    Python = @(
        "${env:SystemDrive}\Python*"
        "${env:SystemDrive}\Tools\Python*"
    )
    Ruby = @(
        "${env:SystemDrive}\Ruby*"
        "${env:SystemDrive}\Tools\Ruby*"
    )
    Vim = @(
        "${env:programfiles}\vim\vim??"
        "${env:programfiles(x86)}\vim\vim??"
    )
    WindowsSDK = @(
        "${env:programfiles(x86)}\Windows Kits\8.1\bin\x64"
        "${env:programfiles(x86)}\Windows Kits\8.0\bin\x64"
        "${env:programfiles(x86)}\Microsoft SDKs\Windows\v7.1A\Bin\x64"
    )
    VisualStudio = @(
        "${env:programfiles(x86)}\Microsoft Visual Studio*"
    )
}
function Get-ExternalBinaryPath {
    param(
        [parameter(mandatory=$true)] [alias("name")] [string] [ValidateScript({
            $ExternalBinaryPathSearchPatterns.keys -contains $_
            })] $BinaryName,
        [switch] $CygwinPath
        #[alias("version")] [int] $MajorVersion
    )
    $ExtantPathPatterns = $ExternalBinaryPathSearchPatterns.$BinaryName |? { test-path $_ }
    if ($ExtantPathPatterns) {
        $foundPath = (get-item $ExtantPathPatterns | sort)[-1].fullname
        if ($CygwinPath) { 
            $foundPath = $foundPath -replace "^(.)\:\\",'\$1\' -replace "\\","/"
        }
        return $foundPath
    }
    else {
        #throw "Could not find a path for $BinaryName"
    }
}

<#
.description
Parse a command line
Return an object that has the command line as an array of arguments, as a string representing the command line, and as a runnable scriptblock 
Intended to be used as plumbing for other commands
.parameter arguments
The arguments (include $0) to parse
.example
Parse-CommandLine New-Item -ItemType File -Value "C:\test.txt"
Returns ...
#>
function Parse-Commandline {
    [cmdletbinding()] param(
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $arguments
    )
    # The point of this is to make sure that things which came in as one argument are represented that way
    # It's not perfect but it gets the job done in 90% of cases
    $argumentString = ($arguments |% { 
        if ($_ -match ' ') { "`"$_`"" }
        else { "$_" }
    }) -join " "
    $ooProps = @{
        Arguments = $arguments
        CommandLine = $argumentString
        Scriptblock = [Scriptblock]::Create($argumentString)
    }
    $outObject = New-Object PSObject -property $ooProps
    return $outObject
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
    $cli = Parse-Commandline $arguments
    write-host "wtf $($cli.arguments)"
}

<#
.synopsis
Echo and execute a single expression
#>
function Echoexec-Expression {
    [cmdletbinding()] param(
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $arguments
    )
    $cli = Parse-Commandline $arguments
    write-host "Echoexec-Expression: $($cli.commandline)"
    $cli.scriptblock.invoke()
}
set-alias echoexec echoexec-expression

function Export-ConemuConfig {
    param(
        [parameter(mandatory=$true)] [string] $filename,
        [switch] $force
    )
    $regKey = "HKCU\Software\ConEmu\.Vanilla"
    $call = 'reg export $regKey "{0}"' -f "$filename"
    if ($force) { $call += " /y" }
    Invoke-Expression $call
}
if (test-path $env:ProgramFiles\ConEmu) {
    set-alias ConEmu64 $env:ProgramFiles\ConEmu\ConEmu64.exe
    set-alias Rename-ConEmuTab $env:ProgramFiles\ConEmu\ConEmu\RenameTab.cmd
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
Import a PFX certificate
.description 
Sets the PersistKeySet flag, which means that you can actually use the fuckin cert later on
(Unlike, say, I dunno, the first-party Import-PfxCertificate function.)
#>
function Import-MrlX509Certificate {
    param(
        [parameter(mandatory=$true)] [string] $Path,
        [parameter(mandatory=$true)] [string] $pfxPassword,
        [string] $StoreLocation = "CurrentUser",
        [string] $StoreName = "My",
        [switch] $exportable,
        [switch] $protected
    )
    $path = resolve-path $path

    $flags = [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::PersistKeySet
    if ($exportable) {
        $flags = $flags -bxor [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::Exportable
    }
    if ($protected) {
        $flags = $flags -bxor [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::UserProtected
    }

    $pfx = new-object System.Security.Cryptography.X509Certificates.X509Certificate2
    $pfx.import($Path, $pfxPassword, $flags)

    $store = new-object System.Security.Cryptography.X509Certificates.X509Store($StoreName, $StoreLocation)
    $store.open('MaxAllowed')
    $store.add($pfx)
    $store.close()
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

# This works. Caveat: Emacs is iffy for some reason. 
# You can 'elevate-process emacs \somefile.txt' just fine
# You can 'elevate-process notepad "\somefile with spaces.txt"'
# But if you 'elevate-process emacs "\somefile with spaces.txt"', Emacs will fail
# I am not sure why. 

# TODO: This should be rewritten based on http://poshcode.org/3158
#       However, use In-CliXml and Out-CliXml instead of shitty temporary files
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

function gcollect {
    [GC]::Collect()
}

$emacsbin = "$Home\opt\emacs-23.4\bin" # this is going to change every time I upgrade Emacs or whatever, ugh
if (test-path $emacsbin) {
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
}

function Show-ScriptContents {
    param(
        [paramater(mandatory=$true)] [string] $commandName
    )
    $contents = @()
    foreach ($c in (get-command $commaneName)) {
        if ($c.path) {
            $contents += $c.path
        }
    }
    return $contents
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


# mklink isn't an exe - it's a cmd.exe builtin! what the fuck. 
# also note that you cannot do this without elevating the prompt first lolololololol
function mklink {
    cmd /c mklink $args
}

function Generate-Password {
    param([int]$length=8)
    # From: http://ronalddameron.blogspot.com/2009/09/two-lines-of-powershell-random.html
    $null = [Reflection.Assembly]::LoadWithPartialName("System.Web")
    [System.Web.Security.Membership]::GeneratePassword($length,2)  # 8 bytes long
}
set-alias pwgen generate-password

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

function llm {
    get-childitem $args | sort-object -property lastwritetime
}
function lse {
    param(
        [string] $path = "."
    )
    $items = get-childitem -include $path -Recurse -Force -ErrorAction SilentlyContinue 
    $enc = $items | Where-Object {$_.Attributes -ge "Encrypted"} 
    $enc.FullName
}

function .. { cd .. }



#### SUBLIME TEXT and LESS

if (test-path alias:more) { del alias:more }
if (test-path function:more) { del function:more }
if (test-path alias:l) { del alias:l }
set-alias l less
$env:LESS = "-iRC"

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

$sublpath = "${env:ProgramFiles}\Sublime Text 3\sublime_text.exe"

if (test-path $sublpath) {
    function subl {
        [cmdletbinding()]
        param(  
            [Parameter(position=0, ValueFromPipeline=$true, ValueFromPipelineByPropertyName=$true)] [string[]] $file
        )
        process {
            foreach($f in $file) {
                start-process $sublpath -argumentlist "`"$f`""
            }
        }
    }
}

# You want this to be separated with forward slashes so that it works
# from the Git (bash) command line and cmd and Powershell etc.
$env:GIT_EDITOR = "$env:SystemRoot\system32\notepad.exe" -replace "\\","/"

# You'll want to turn off colors for git diffs - `git config --global color.diff false` - b/c git colors are ANSI escapes and 
# Vim doesn't understand those (at least not out of the box)
$vimCygwinPath = Get-ExternalBinaryPath -BinaryName vim -CygwinPath
if ($vimCygwinPath) { 
    $env:GIT_PAGER = '"{0}/vim.exe" --cmd "let no_plugin_maps = 1" -c "runtime! macros/less.vim" -' -f $vimCygwinPath
}

function Get-GitPrettyLog {
    git log --oneline --all --graph --decorate $args
}
set-alias gpl Get-GitPrettyLog
function Compare-GitObjectsOnGitHub {
    param(
        [parameter(mandatory=$true)] [string] $CommitA,
        [parameter(mandatory=$true)] [string] $CommitB
    )
    $remotes = git remote -v
    foreach ($r in $remotes) {
        $name,$address,$direction = $r -split "\s+"
        if (($name -eq "origin") -and ($direction -eq "(fetch)")) {
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
    param(
        [parameter(mandatory=$true)] [string[]] $filePath,
        [int] $bytes = 2
    )
    $maxBytes = $bytes - 1
    $ret = @()
    foreach ($fp in $filePath) {
        $MagicNumberContainer = new-object PSObject
        $fullName = (get-item $fp).fullname
        $firstBytes = [System.IO.File]::ReadAllBytes($fullName)[0..$maxBytes]
        $firstBytesHex = $firstBytes |% { "0x{0:X0}" -f $_ }

        $MagicNumberContainer | Add-Member -MemberType NoteProperty -Name FullName -Value $fp 
        $MagicNumberContainer | Add-Member -MemberType NoteProperty -Name FirstBytes -Value $firstBytes
        $MagicNumberContainer | Add-Member -MemberType NoteProperty -Name FirstBytesHex -Value $firstBytesHex
        $ret += @($MagicNumberContainer)
    }
    return $MagicNumberContainer
}

function Get-Profiles {
    [cmdletbinding()]
    param(
        [string] $containingPattern,
        [switch] $caseSensitive
    )
    $profiles = gci -recurse $dhdbase\opt\powershell -include *.ps1,*.psm1
    foreach ($p in $profile.AllUsersAllHosts,$profile.AllUsersCurrentHost,$profile.CurrentUserAllHosts,$profile.CurrentUserCurrentHost,$profile.dhd) {
        if (test-path $p) { $profiles += @(get-item $p) }
    }

    if ($containingPattern) {
        write-verbose "Searching through results files for strings matching '$containingPattern'..."
        $results = $profiles |? { $_ | sls -quiet -pattern $containingPattern -caseSensitive:$caseSensitive }
    }
    else {
        $results = $profiles
    }
    return $results
}
set-alias gpro Get-Profiles

function whoami {
    $me.identity.name
}
function id {
    $output = "" + $me.identity.name + "(" + $me.identity.user.value + ")"
    $output
}

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
        if (get-module PSCX) {
            get-help $a -full | & "$Pscx:Home\Apps\less.exe"
        }
        else {
            get-help $a -full | less
        }
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
if (test-path alias:help) { del alias:help }
set-alias help get-help
if (test-path function:help) { del function:help } # PSCX has one of these
if (test-path function:get-help) { del function:get-help } # PSCX has one of these

# Set-LocationEx from PSCX let's you move back/forward in your location stack with these:
#     Set-LocationEx -
#     Set-LocationEx +
# Calling just Set-LocationEx displays the stack
if (test-path alias:pwd) { del alias:pwd }
if (test-path alias:cd) { del alias:cd }
if (get-module pscx) {
    function pwd { Set-LocationEx | select -last 5 }
    function cd {
        param(
            [parameter(position=0, valuefrompipeline=$true)] $location = $home
        )
        Set-LocationEx $location
    }
}
else {
    function pwd { Get-Location }
    function cd {
        param(
            [parameter(position=0, valuefrompipeline=$true)] $location = $home
        )
        Set-Location $location
    }
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

    switch ($windowSylte) { 
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

$startmenu="$env:appdata\Microsoft\Windows\Start Menu"

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
        #[Parameter(ParameterSetName='shortcut')] [alias("f")] [switch]$force, 
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
    switch ($pscmdlet.ParameterSetName) {
        "shortcut" { 
            $a = @{filename = $source
                   target = $target
                   arguments = $arguments
                   force = $force 
            }
            New-MRLShortcut @a 
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

function Get-SystemPath {
    ($env:path).split(";")
}

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
        if (-not $SoyAdmin) {
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
        if (-not $SoyAdmin) {
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
        Rename-Tab $hostname
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

$StartMenu = New-Object PSObject
Add-Member -Force -InputObject $StartMenu -MemberType NoteProperty -Name CurrentUser -Value "${env:AppData}\Microsoft\Windows\Start Menu\"
Add-Member -Force -InputObject $StartMenu -MemberType NoteProperty -Name AllUsers -Value "${env:ProgramData}\Microsoft\Windows\Start Menu\"

set-alias getmo Get-Module
set-alias rmmo Remove-Module

function Get-Clipboard {
    [cmdletbinding()]
    param(
        [switch] $AsArray
    ) 
    Add-Type -Assembly PresentationCore
    $clipboardText = [Windows.Clipboard]::GetText()
    if ($AsArray) {
        $clipboardText = $clipboardText -replace "`r",'' -split "`n"
    }
    return $clipboardText
}
function Set-ClipboardFucked {
    [cmdletbinding()]
    param(
        [string] $text,
        [switch] $Append
    )
    throw "This is broken right now actualy"
    Add-Type -AssemblyName 'System.Windows.Forms'
    $str = $input | Out-String
    if (-not $Append) {
        [Windows.Forms.Clipboard]::Clear()
    }
    if ($str) {
        [Windows.Forms.Clipboard]::SetText($str)
    }
}

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
    [cmdletbinding(DefaultParameterSetName='FromText')]
    param(
        [parameter(mandatory=$true,ParameterSetName='FromText')] [string] $text,
        [parameter(mandatory=$true,ParameterSetName='FromFile')] [string] $fileName,
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

function Wrap-Text {
    [cmdletbinding()] 
    param(
        [parameter(mandatory=$true)] [string] $text,
        [parameter(mandatory=$true)] [int] $width,
        [int] $indentSpaces = 0
    )
    $width = $width -1
    if ($indentSpaces -ge $width) {
        throw "`$indentSpaces must be smaller than `$width"
    }
    $indent = " " * $indentSpaces
    $output = ""
    $ctr=0
    foreach ($line in ($text -split "`n")) {
        $ctr+=1
        #write-host -foreground cyan "${ctr}: $line"

        $finished = $false
        while (-not $finished) {
            $line = "$indent$line"
            if ($line.length -gt $width) {
                $output += $line.substring(0,$width)
                $output += "`n"
                $line = $line.substring($width)
            }
            else {
                $output += $line
                $output += "`n"
                $finished = $true
            }
        }

        if ($output[-1] -ne "`n") {
            $output += "`n"
        }
    }
    return $output
}

function Show-ErrorReport {
    [cmdletbinding()]
    param(
        [switch] $ExitIfErrors
    )
    write-verbose "`$error.count = $($error.count)"
    write-verbose "`$LASTEXITCODE = $LastExitCode"    

    if ($Host -and $Host.UI -and $Host.UI.RawUI) {
        $wrapWidth = $Host.UI.RawUI.Buffersize.Width
    }
    else {
        $wrapWidth = 9999
    }

    $doExit = $false
    $reportString = "ERROR Report: No errors`n"

    #$allErrors = $global:error.ToArray() + $script:error.ToArray() + $local.error.ToArray()

    <#
    foreach ($schope in "global","script","local") {
        $scopedErrorVar = get-variable -scope $scope -name error
        $scopedErrorArray = @()
        if ($scopedErrorVar.count) { 
            $scopedErrorArray = $scopedErrorVar.ToArray()
        }
        foreach ($e in $scopedErrorArray) {
            if ($e.Scope -match $scope) { 
                write-verbose "Scope of '$scope' already exists for "
            }
        }
    }
    #>

    if ($error.count -or $LASTEXITCODE) {
        $errorSummary = "`$LASTEXITCODE=$LastExitCode, `$Error.count=$($Error.count)"
        $reportString = "ERROR Report: $errorSummary`n`n"

        for ($i= $error.count -1; $i -ge 0; $i -= 1) {
            write-verbose "Processing error $i"
            $e = $error[$i]

            $errorDetails  = "PS `$Error[$i]: `n"
            $indentCount = 4

            # $error can contain at least 2 kinda of objects - ErrorRecord objects, and things that wrap ErrorRecord objects
            # The information we need is found in the ErrorRecord objects, so unwrap them here if necessary
            if ($e.PSObject.Properties['ErrorRecord']) { # This looks weird but it makes it work with strict mode
                $e = $e.ErrorRecord
            }

            $errorDetails += Wrap-Text -text $e.ToString() -width $wrapWidth -indent $indentCount
            if ($errorDetails[-1] -ne "`n") { $errorDetails += "`n" }

            if ($e.ScriptStackTrace) {
                $errorDetails += wrap-text -text $e.ScriptStackTrace -width $wrapWidth -indent $indentCount
                if ($errorDetails[-1] -ne "`n") { $errorDetails += "`n" }
            }

            $reportString += $errorDetails
        }

        if ($ExitIfErrors) { 
            $doExit = $true
            $reportString += "Exiting with returncode 1...`n" 
        }
    }

    write-output "----`n$reportString----"
    if ($doExit) { 
        exit 1
    }
}
set-alias err Show-ErrorReport

function Clear-Error {
    $error.clear()
    $global:LASTEXITCODE = 0
}
set-alias clerr Clear-Error

set-alias gj Get-Job
set-alias jobs Get-Job
set-alias recj Receive-Job
set-alias rj Receive-Job
set-alias rmj Remove-Job
set-alias resj Resume-Job
set-alias sj Start-Job
set-alias stopj Stop-Job
set-alias susj Suspend-Job
set-alias wj Wait-Job
# Job-related TODOs: 
# - Show in prompt if I have un-received jobs
# - Show im prompt complete/incomplete status of jobs 
# - Function to get output from all completed jobs
# - Maybe wrap receive-job to get all available output by default? 


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
        [string] $outDir = "$pwd",
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
            [parameter(mandatory=$true,parametersetname="outdir")] [System.IO.DirectoryInfo] $outDir,
            [parameter(mandatory=$true,parametersetname="nooutdir")] [switch] $noOutDir
        )

        if ($noOutDir) { 
            $outdir = $archive.directory.fullname
            $feTempDir = $outdir
        }
        else {
            $outdir = (get-item $outdir).fullname
            $feTempDir = "$outDir\fuckingextract-$([System.IO.Path]::GetRandomFileName())"
            if (test-path $feTempDir) {
                throw "The temporary directory that already exists"
            }
            mkdir $feTempDir | out-null
        }

        $7zcmd = "7z x `"-o$feTempDir`" `"$($archive.fullname)`""

        $7zout = iex $7zcmd

        if ($LASTEXITCODE -ne 0) {
            throw ("7z exited with code $LASTEXITCODE",
                "`n`tcommand line: $7zcmd",
                "`n`toutput: `n$7zout")
        }

        return $feTempDir
    }

    try {
        gcm 7z | out-null
    }
    catch {
        throw "7z.exe is not in your `$ENV:PATH; cannot continue"
    }

    $secondLayerExtensions = @(".tar") # There aren't any more that I can think of?

    if (-not (test-path $outDir)) {
        mkdir -force $outDir | out-null
    }
    $outdir = get-item $outdir

    $outFiles = @()
    foreach ($arch in $archive) {
        $archItem = get-item $arch

        # this is the name of the archive w/o its extension
        # this will be used as the eventual directory name to extract to 
        $archBareName = [System.IO.Path]::GetFileNameWithoutExtension($archItem.name)

        $exDir = fuckingExtractOneLayer -archive $archItem -outdir $outdir
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
        # If there's more than one, then rename the dir to the bare name of the
        # archive
        # TODO: add a -force flag 
        if ($exItems.count -eq 1) {
            $outItem = $exItems[0]
            $outItemName = "$($outdir.fullname)\$($outItem.name)"
            write-verbose "Only one item in archive: '$($outItem.fullname)'; moving to '$($outdir.fullname)'"

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
        else {
            $outItemName = "$($outdir.fullName)\$archBareName"
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
The System.Environment+SpecialFolders enum has a lot of really useful stuff 
such as: 
- StartMenu / CommonStartMenu: the start menu folder for my user / all users
- StartUp / CommonStartUp: the startup folder for my user / all users
... and lots more. This makes a hashtable from that, so it's easier to access
#>
$SpecialFolders = New-Object PSObject
foreach ($sf in [system.Enum]::GetValues([System.Environment+SpecialFolder])) {
    $sfpath = [Environment]::GetFolderPath($sf)
    add-member -inputobject $SpecialFolders -membertype NoteProperty -name $sf -value $sfpath -force
}

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
'docker-machine env <name>' returns some Bash export functions. Translate these to Powershell.
#>
function Parse-DockerMachineEnv {
    [CmdletBinding()] param(
        [Parameter(Mandatory=$true,ValueFromPipeline=$true)] [String] $env
    )
    process {
        if ($env -match "^\s*#") {} #comment
        elseif ($env -match "^\s*export (?<name>[a-zA-Z0-9_]+)\=`"(?<value>.*)`"") {
            Set-WinEnvironmentVariable -name $matches.name -value $matches.value #-verbose:$verbose
        }
        else {
            throw "I don't know how to parse this input"
        }
    }
}

function Apply-DockerMachineEnv {
    [CmdletBinding()] param(
        [Parameter(Mandatory=$true)] [String] $machineName
    )
    $env = docker-machine env "$machineName"
    $env | Parse-DockerMachineEnv
    $env
}