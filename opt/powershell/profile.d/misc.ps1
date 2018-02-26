#### Miscellaneous / Everything ####


$SpecialCharacters = New-Object PSObject -Property @{
    Beep         = [char]7      # beeps @ u
    DoublePrompt = [char]187    # » (RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK)
    Lambda       = [char]955    # λ (GREEK LETTER LAMBDA)
    HammerSickle = [char]9773   # ☭ (HAMMER AND SICKLE)
    VisualStudio = [char]42479  # ꗯ (VAI SYLLABLE GBE)
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

function conkeror {
    $xulrunnerbin = $home + "\opt\xulrunner\xulrunner.exe"
    & $xulrunnerbin  "$home\opt\src\conkeror\application.ini" $args
}

function New-Password {
    param([int]$length=8)
    # From: http://ronalddameron.blogspot.com/2009/09/two-lines-of-powershell-random.html
    $null = [Reflection.Assembly]::LoadWithPartialName("System.Web")
    [System.Web.Security.Membership]::GeneratePassword($length,2)  # 8 bytes long
}
Set-Alias -Name pwgen -Value New-Password

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

function Import-ModuleIdempotently {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name
    )
    $module = Get-Module -Name $Name
    if ($module) {
        Write-Verbose -Message "Module is already imported. Removing and re-adding..."
        Remove-Module -Name $Name
        Import-Module -Name $module.Path
    } else {
        Write-Verbose -Message "Module was not imported. Trying to add module by name '$Name'..."
        Import-Module -Name $Name
    }
}
Set-Alias -Name Reimport-Module -Value Import-ModuleIdempotently
Set-Alias -Name reimport -Value Import-ModuleIdempotently

Set-Alias -Name getmo -Value Get-Module
Set-Alias -Name rmmo -Value Remove-Module

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
