# [char]955     λ (GREEK LETTER LAMBDA)
# [char]9773    ☭ (HAMMER AND SICKLE)
# [char]42479   ꗯ (VAI SYLLABLE GBE)
# [char]1003    ϫ (COPTIC SMALL LETTER GANGIA)
# [char]7       beeps @ u
$LambdaChar = "$([char]955)"
$HammerAndSickleChar = "$([char]9773)"
$VisualStudioChar = "$([char]42479)"
$BeepChar = @([char]7)


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

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}

$possibleOpenSSL = @(
    "${env:programfiles}\OpenSSL-Win64\bin\openssl.exe"
    "${env:programfiles(x86)}\Git\bin\openssl.exe"
    'C:\STRAWBERRY\C\BIN\openssl.exe'
)
foreach ($o in $possibleOpenSSL) {
    if (test-path $o) {
        set-alias OpenSslExe $o
        $env:OPENSSL_CONF="$Home\.dhd\opt\win32\openssl.cnf"
        break
    }
}
function Invoke-OpenSSL {
    [cmdletbinding()]
    param(
        [string[]] $argumentList = @(),
        [switch] $Passthru
    )
    $OpenSslPath = (gcm OpenSslExe |? {$_.commandtype -eq 'Alias'}).definition
    $sslProc = Invoke-ProcessAndWait -RedirectStandardError -RedirectStandardOutput -command $OpenSslPath -argumentList $argumentList

    $stdout = $sslProc.StandardOutput.ReadToEnd()
    $stderr = $sslProc.StandardError.ReadToEnd()

    write-verbose "===== Standard Output ====="
    write-verbose $stdout
    write-verbose "===== Standard  Error ====="
    write-verbose $stderr

    # In normal mode, always display output. 
    # In Passthru mode, only display output if the ExitCode was not zero

    if ($Passthru) {
        if ($sslProc.ExitCode -ne 0) {
            if ($stdout) { write-host $stdout }
            if ($stderr) { write-host $stderr -foregroundcolor Red }
            throw "OpenSSL exited with code '$($sslProc.ExitCode)'"
        }
        # Necessary because the .ReadToEnd() method can't get called more than once
        $sslProc | Add-Member -MemberType NoteProperty -Name SerializedStandardOutput -Value $stdout
        $sslProc | Add-Member -MemberType NoteProperty -Name SerializedStandardError -Value $stderr
        return $sslProc
    }
    else {
        if ($stdout) { write-host $stdout }
        if ($stderr) { write-host $stderr -foregroundcolor Red }
    }
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
    .description
    This is a workaround for a stupid idiot bug in start-process that only triggers sometimes.
    http://social.technet.microsoft.com/Forums/scriptcenter/en-US/37c1066e-b67f-4709-b195-aa2790216bd0
    https://connect.microsoft.com/PowerShell/feedback/details/520554/
    The bug has it return instantly even when -wait is passed to start-process, at least on Eric's local box. 
    When that happens, $process.ExitCode hasn't been populated, and won't be, even when the process does actually exit.
    System.Diagnostics.Process doesn't have that behavior, so that's what we're going to use instead

    This also lets me redirect stderr and stdout and get them in the $process object
#>
function Invoke-ProcessAndWait {
    [cmdletbinding()]
    param(
        [parameter(mandatory=$true)] [string] $command,
        [parameter(mandatory=$true)] [string[]] $argumentList,
        [switch] $RedirectStandardError,
        [switch] $RedirectStandardOutput
    )
    #write-verbose "Running '$command' with arguments '$argumentList'"
    write-verbose "$command $($argumentList -join " ")"
    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = New-Object System.Diagnostics.ProcessStartInfo
    $process.StartInfo.FileName = $command
    $process.StartInfo.RedirectStandardError = $RedirectStandardError
    $process.StartInfo.RedirectStandardOutput = $RedirectStandardOutput
    $process.StartInfo.UseShellExecute = $false # AKA don't run in a new window
    $process.StartInfo.WorkingDirectory = $pwd
    $process.StartInfo.Arguments = $argumentList
    $process.Start() | Out-Null
    $process.WaitForExit()
    return $process
}


# note: 7-zip is in the same place on both 64 bit and 32 bit Windows
# note: in some cases it won't complete commands starting with a digit, so we are reduced to this
set-alias sz "$env:programfiles\7-Zip\7z.exe" 

#if (test-path C:\Chocolatey) {
#    set-alias nuget C:\Chocolatey\chocolateyinstall\nuget.exe
#}

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


function .. { cd .. }



#### SUBLIME TEXT and LESS

if (test-path alias:more) { del alias:more }
if (test-path function:more) { del function:more }
if (test-path alias:l) { del alias:l }
set-alias l less
$env:LESS = "-iRC"

$possibleVimDirs = @()
if (test-path "${env:programfiles(x86)}\vim") {
    $possibleVimDirs += @((ls "${env:programfiles(x86)}\vim\vim??").fullname | sort-object -descending)[0]
}
if (test-path "${env:programfiles}\vim") {
    $possibleVimDirs += @((ls "${env:programfiles}\vim\vim??").fullname | sort-object -descending)[0]
}
foreach ($vd in $possibleVimDirs) {
    if ($vd) {
        $VimHome = $vd
        set-alias vim $VimHome\vim.exe
        function vless {
            # Adapted from vim/macros/less.bat
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
        break
    }
}

<#
$possibless = @(
    "$home\Documents\WindowsPowerShell\Modules\PSCX\Apps\less.exe"
    "C:\opt\MinGW\msys\1.0\bin\less.exe",
    "${env:ProgramFiles(x86)}\Git\bin\less.exe",
    "$env:windir\system32\more.com"
)
foreach ($pl in $possibless) {
    if (test-path $pl) {
        set-alias less "$pl"
        set-alias l less
        set-alias more less
        set-alias m less
        break
    }
}

###### I think this is superseeded by PSCX's less function
# this will only work with a decent less.exe as described above
# it's intended so you can do something like 
#     gci 'c:\program files' -include *.txt | lessall
# like I do with my subl() function 
# TODO: this is using PSCX's less.exe by calling 'less.exe' directly. 
#       it won't work with the one from Git. Ugh. 
function lessall {
    $files = @()
    foreach ($f in $input) { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
    foreach ($f in $args)  { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
    $allfiles = $files -join " "
    less.exe $allfiles
}
#>

$sublpath = "C:\Program Files\Sublime Text 3\sublime_text.exe"

if (test-path $sublpath) {
    #set-alias subl "$sublpath"
    function subl_OLDVERSION_FASTER_BROKEN {
        [cmdletbinding()]

        param(  
            [Parameter(position=0, ValueFromPipeline=$true, ValueFromPipelineByPropertyName=$true)] [string[]] $file
        )

        $files = @()
        #foreach ($f in $input,$args) { 
        foreach ($f in $file) {
            if (-not [string]::IsNullOrEmpty($f)) { 
                if ($f.fullname) {
                    # This is probably a FileInfo object
                    $ff = $f.fullName
                    write-verbose "Using fileinfo object at full path $ff"
                    $files += @("`"$ff`"")
                }
                else {
                    # Assume it's just a string
                    write-verbose "Using a string as $f"
                    $files += @("`"$f`"") 
                }
            } 
        }
        foreach ($f in $args)  { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
        start-process $sublpath -argumentlist $files
        write-host $files
    }
    function subl {
        [cmdletbinding()]
        param(  
            [Parameter(position=0, ValueFromPipeline=$true, ValueFromPipelineByPropertyName=$true)] [string[]] $file
        )
        process {
            foreach($f in $file) {
                start-process $sublpath -argumentlist "$f"
            }
        }
    }
}
#$env:GIT_EDITOR = $sublpath
#$env:SVN_EDITOR = $sublpath
#$env:EDITOR = $sublpath
#$env:VISUAL = $sublpath

# You want this to be separated with forward slashes so that it works
# from the Git (bash) command line and cmd and Powershell etc.
$env:GIT_EDITOR = "$env:SystemRoot\system32\notepad.exe" -replace "\\","/"

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

function Get-FileEncoding {
    ##############################################################################
    ##
    ## Get-FileEncoding
    ##
    ## From Windows PowerShell Cookbook (O'Reilly)
    ## by Lee Holmes (http://www.leeholmes.com/guide)
    ##
    ##############################################################################

    <#

    .SYNOPSIS

    Gets the encoding of a file

    .EXAMPLE

    Get-FileEncoding.ps1 .\UnicodeScript.ps1

    BodyName          : unicodeFFFE
    EncodingName      : Unicode (Big-Endian)
    HeaderName        : unicodeFFFE
    WebName           : unicodeFFFE
    WindowsCodePage   : 1200
    IsBrowserDisplay  : False
    IsBrowserSave     : False
    IsMailNewsDisplay : False
    IsMailNewsSave    : False
    IsSingleByte      : False
    EncoderFallback   : System.Text.EncoderReplacementFallback
    DecoderFallback   : System.Text.DecoderReplacementFallback
    IsReadOnly        : True
    CodePage          : 1201

    #>

    param(
        ## The path of the file to get the encoding of.
        $Path
    )

    Set-StrictMode -Version Latest

    ## The hashtable used to store our mapping of encoding bytes to their
    ## name. For example, "255-254 = Unicode"
    $encodings = @{}

    ## Find all of the encodings understood by the .NET Framework. For each,
    ## determine the bytes at the start of the file (the preamble) that the .NET
    ## Framework uses to identify that encoding.
    $encodingMembers = [System.Text.Encoding] |
        Get-Member -Static -MemberType Property

    $encodingMembers | Foreach-Object {
        $encodingBytes = [System.Text.Encoding]::($_.Name).GetPreamble() -join '-'
        $encodings[$encodingBytes] = $_.Name
    }

    ## Find out the lengths of all of the preambles.
    $encodingLengths = $encodings.Keys | Where-Object { $_ } |
        Foreach-Object { ($_ -split "-").Count }

    ## Assume the encoding is UTF7 by default
    $result = "UTF7"

    ## Go through each of the possible preamble lengths, read that many
    ## bytes from the file, and then see if it matches one of the encodings
    ## we know about.
    foreach($encodingLength in $encodingLengths | Sort -Descending)
    {
        $bytes = (Get-Content -encoding byte -readcount $encodingLength $path)[0]
        $encoding = $encodings[$bytes -join '-']

        ## If we found an encoding that had the same preamble bytes,
        ## save that output and break.
        if($encoding)
        {
            $result = $encoding
            break
        }
    }

    ## Finally, output the encoding.
    [System.Text.Encoding]::$result
}

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


if (test-path "${env:ProgramFiles(x86)}\Git\bin\diff.exe") {
    set-alias unixdiff "${env:ProgramFiles(x86)}\Git\bin\diff.exe"
}
if (test-path "${env:ProgramFiles(x86)}\Git\bin\sed.exe") {
    set-alias unixsed "${env:ProgramFiles(x86)}\Git\bin\sed.exe"
}

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


if (test-path alias:man) { del alias:man }
function man {
    foreach ($a in $args) {
        get-help $a -full | less
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
<#
function help {
    foreach ($a in $args) {
        (get-help $a).syntax
    }
}
#>

if (test-path alias:cd) { del alias:cd }
# You can pipe a path to this 
# This is particularly useful for something like `mkdir asdf | cd` to mkdir/cd in the same command, nice

# Set-LocationEx from PSCX let's you move back/forward in your location stack with these:
#     Set-LocationEx -
#     Set-LocationEx +
# Calling just Set-Location displays the stack
$setloc = "set-location"
if (get-module pscx) {
    $setloc = "Set-LocationEx"
    if (test-path alias:pwd) { del alias:pwd }
    set-alias pwd $setloc
}

function cd {
    param(
        [parameter(position=0, valuefrompipeline=$true)] $location = $home
    )
    iex "$setloc `"$location`""
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
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = @("Machine","User","Process")
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
    "",$keydata | plink $hostname "mkdir -p ~/.ssh && cat - >> $akeys && chmod 700 ~/.ssh && chmod 600 $akeys"
}

$bvssh = "${env:ProgramFiles(x86)}\Bitvise SSH Client"
if (test-path $bvssh) {
    function Invoke-BitviseSsh {
        $stermcArguments = $args + @("-keypairFile=$Home\.ssh\id_rsa")
        $stermcArguments+= @("-keypairFile=$Home\.ssh\id_rsa")
        $stermcArguments+= @("-hostKeyFile=$Home\.dhd\hbase\known_hosts")
        start-process -wait -nonewwindow $bvssh\stermc.exe -argumentList $stermcArguments
    }
    set-alias ssh Invoke-BitviseSsh
    function Get-BitviseKnownHosts {
        [cmdletbinding()]
        param(
            [string] $hostname
        )
        $HostKeysReg = get-item hkcu:\Software\Bitvise\HostKeys
        $HostKeys = @{}
        foreach ($entry in ($HostKeysReg.GetValueNames() |where {$_.StartsWith("HostKey2_")} )) {
            write-debug $entry

            #$newHost = @{}
            #$newHost.Fingerprint = ($entry -split '_')[3][0..31] -join ""
            #write-verbose "Fingerprint: $($newHost.Fingerprint)"
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
            write-debug ($binHostname -join ",")
            #$newHost.Hostname = (New-Object System.Text.ASCIIEncoding).GetString($binHostname)
            #write-verbose "Hostname: $($newHost.Hostname)"
            #$HostKeys += @($newHost)
            $asciiHostname = (New-Object System.Text.ASCIIEncoding).GetString($binHostname)
            write-verbose "Hostname: $asciiHostname"
            $HostKeys.$asciiHostname = $fingerprint
        }
        if ($hostname) {
            if ($hostKeys.$hostname) {
                return @{ $hostname = $hostKeys.$hostname }
            }
            else {
                throw "No host key for hostname $hostname"
            }
        }
        else {
            return $hostKeys
        }
    }
}

foreach ($exe in (gci "$env:programfiles\ShrewSoft\VPN Client\*.exe")) {
    set-alias $exe.basename $exe.fullname
}
