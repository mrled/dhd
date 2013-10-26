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

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}

$possibleOpenSSL = @(
    'C:\Program Files\OpenSSL-Win64\bin\openssl.exe',
    'C:\STRAWBERRY\C\BIN\openssl.exe'
)
foreach ($o in $possibleOpenSSL) {
    if (test-path $o) {
        set-alias openssl $o
        break
    }
}

# note: 7-zip is in the same place on both 64 bit and 32 bit Windows
# note: in some cases it won't complete commands starting with a digit, so we are reduced to this
set-alias sz "$env:programfiles\7-Zip\7z.exe" 

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

# char7 is the beep sound. you can just type $beep and it will beep at you.
$beep = @([char]7)

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
