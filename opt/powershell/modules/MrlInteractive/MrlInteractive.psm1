<#
.SYNOPSIS
A module template
#>

<#
.SYNOPSIS
Show the input commandline
.DESCRIPTION
OH MY GOD
Type a command, change your mind about it, move the cursor to the front of the line, type "omg ",
and hit return. Blammo, it returns "wtf <the command line you typed>"
.NOTES
Intended to be aliased to 'omg'
#>
function Show-InputCommandline {
    [CmdletBinding()] Param(
        [Parameter(Position=0, ValueFromRemainingArguments=$true)] $Arguments,
        $Prefix = "wtf:"
    )
    # Wrap $Prefix in <# #> so you can copy/paste the whole line
    # Use $Arguments -join to get around any custom $OFS settings
    Write-Output -InputObject "<# $Prefix #> $($Arguments -join ' ')"
}

<#
.SYNOPSIS
Create a new password
.PARAMETER Length
The length of the password
#>
function New-Password {
    Param(
        [int] $Length = 16
    )
    # From: http://ronalddameron.blogspot.com/2009/09/two-lines-of-powershell-random.html
    [Reflection.Assembly]::LoadWithPartialName("System.Web") | Out-Null
    [System.Web.Security.Membership]::GeneratePassword($length,2)
}

<#
.SYNOPSIS
Get child items sorted by last write time
.NOTES
Intended to be aliased to 'llm', analogous to my bash function of the same name
#>
function Get-ChildItemSortedLastWriteTime {
    Get-ChildItem $args | Sort-Object -Property LastWriteTime
}

<#
.SYNOPSIS
Change to parent directory
.NOTES
Intended to be aliased to '..'
#>
function Set-LocationParent {
    [CmdletBinding()] Param()
    Set-Location -Path ..
}

<#
.SYNOPSIS
Change to grandparent directory
.NOTES
Intended to be aliased to '...'
#>
function Set-LocationGrantParent {
    [CmdletBinding()] Param()
    Set-Location -Path ../..
}

<#
.SYNOPSIS
Change to great grandparent directory
.NOTES
Intended to be aliased to '....'
#>
function Set-LocationGreatGrandParent {
    [CmdletBinding()] Param()
    Set-Location -Path ../../..
}

<#
.SYNOPSIS
Invoke vim's less macro
#>
function Invoke-VimLessMacro {
    # Adapted from vim/macros/less.bat. Assumes vim is in path though.
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$True, ValueFromPipeline=$True)] [string] $Filename
    )
    if ($input) {
        $input | vim --cmd "let no_plugin_maps = 1" -c "runtime! macros/less.vim" -
    } else {
        vim --cmd "let no_plugin_maps = 1" -c "runtime! macros/less.vim" "$Filename"
    }
}

<#
.SYNOPSIS
Show all definitions of a command
.PARAMETER Command
The name of the command(s) to display
.PARAMETER Recurse
Show definitions recursively - if a command resolves to an alias, also show the definition of the
alias target.
.PARAMETER RecursionLevel
Used when the function calls itself recursively - do not pass this parameter
.NOTES
Intended to be aliased to 'wh'
#>
function Show-AllCommands {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true, Position=0, ValueFromRemainingArguments=$true)]
        [String[]] $Command,

        [Alias("r","a","all")] [switch] $Recurse,

        [int] $RecursionLevel = 0
    )
    foreach ($cmdName in $Command) {
        $level = $RecursionLevel
        if ($level -gt 20) {
            throw "Recursion is greater than 20 levels deep. Probably a circular set of aliases?"
        }
        $levelprefix = ""
        for ($i=0; $i -le $level; $i++) {
            if ($i -eq $level) {
                $levelprefix += "-> "
            } else {
                $levelprefix += "   "
            }
        }

        foreach ($c in @(Get-Command -All -Name $cmdName)) {
            if ($c.CommandType) { #sometime get-command passes us an empty object! awesome!!
                switch ($c.CommandType) {
                    "Alias" {
                        Write-Output -InputObject "${levelprefix}$($c.Name): Aliased to $($c.Definition)"
                        if ($recurse.ispresent) {
                            Show-AllCommands -Command $c.Definition -Recurse -RecursionLevel ($level + 1)
                        }
                    }
                    "Application" {
                        Write-Output -InputObject "${levelprefix}$($c.Name): Executable at $($c.Definition)"
                    }
                    "Function" {
                        # TODO: don't display function definition unless I do -recurse
                        # Can I still show just the parameters though? Hmm.
                        Write-Output -InputObject "${levelprefix}$($c.Name): $($c.CommandType)"
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
                        $re_firstnewline = New-Object -TypeName $regex -ArgumentList ('\A\r?\n', $reml)
                        $re_lastnewline = New-Object -TypeName $regex -ArgumentList ('\Z\r?\n', $reml)
                        $re_newline = New-Object -TypeName $regex -ArgumentList ('\r?\n', $reml)
                        $re_stringbegin = New-Object -TypeName $regex -ArgumentList ('\A', $reml)

                        $functionprefix = $levelprefix + "   " #indent the funct definitions a bit further
                        $defstr = $re_firstnewline.replace($defstr, '')
                        $defstr = $re_lastnewline.replace($defstr, '')
                        $defstr = $re_newline.replace($defstr, [environment]::NewLine + $functionprefix)
                        $defstr = $re_stringbegin.replace($defstr, $functionprefix)

                        Write-Output -InputObject $defstr
                    }
                    default {
                        Write-Output -InputObject "${levelPrefix}$($c.Name): $($c.CommandType)"
                    }
                }
            }
        }
    }
}

<#
.SYNOPSIS
"Touch" a file
.NOTES
Intended to be aliased to 'touch'
#>
function Set-MrlFile {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] [string[]] $Path,
        [DateTime] $Date = (Get-Date)
    )
    foreach ($f in $file) {
        if (Test-Path -Path $f) {
            $item = Get-Item -Path $f
            $item.LastWriteTime = $Date
        }
        else {
            New-Item -ItemType File -Path $f
        }
    }
}

<#
.SYNOPSIS
Get help in an interactively useful format

.DESCRIPTION
Get the help for a command in an interactively useful format, and pipe through less.exe for easy reading.
Parameters are proxied to the Microsoft.PowerShell.Core\Get-Help cmdlet, but the parameter -Full is always passed.
#>
function Get-MrlHelp {
    [CmdletBinding(DefaultParameterSetName='AllUsersView', HelpUri='https://go.microsoft.com/fwlink/?LinkID=113316')]
    param(
        [Parameter(Position=0, ValueFromPipelineByPropertyName=$true)]
        [ValidateNotNullOrEmpty()]
        [string]
        ${Name},

        [string]
        ${Path},

        [ValidateSet('Alias','Cmdlet','Provider','General','FAQ','Glossary','HelpFile','ScriptCommand','Function','Filter','ExternalScript','All','DefaultHelp','Workflow','DscResource','Class','Configuration')]
        [string[]]
        ${Category},

        [string[]]
        ${Component},

        [string[]]
        ${Functionality},

        [string[]]
        ${Role}
    )

    begin {
        $outBuffer = $null
        if ($PSBoundParameters.TryGetValue('OutBuffer', [ref]$outBuffer)) {
            $PSBoundParameters['OutBuffer'] = 1
        }
        $PSBoundParameters['Full'] = $true
        $wrappedCmd = $ExecutionContext.InvokeCommand.GetCommand('Microsoft.PowerShell.Core\Get-Help', [System.Management.Automation.CommandTypes]::Cmdlet)
        $scriptCmd = { & $wrappedCmd @PSBoundParameters | less.exe }
        $steppablePipeline = $scriptCmd.GetSteppablePipeline($myInvocation.CommandOrigin)
        $steppablePipeline.Begin($PSCmdlet)
    }

    process {
        $steppablePipeline.Process($_)
    }

    end {
        $steppablePipeline.End()
    }
}

<#
.SYNOPSIS
Get the syntax for a command
.DESCRIPTION
If you do (Get-Help Something).Syntax, it will return just the syntax for the command. Yay.
... Unless it's a function without a documentation block. Then it returns an ugly SyntaxItem object.
It's mostly the same thing, but if a PSObject is of type `MamlCommandHelpInfo#syntax`, then it
displays is properly. All this does is check to see if the .Syntax object you get from Get-Help
contains that type; if it doesn't, it adds it before returning it.
#>
function Get-Syntax {
    [CmdletBinding()] Param(
        [string[]] $Command
    )
    foreach ($cmd in $Command) {
        $cmdSyntax = (Get-Help -Name $cmd).Syntax
        if (-not $cmdSyntax.PSObject.TypeNames.Contains("MamlCommandHelpInfo#syntax")) {
            $cmdSyntax.PSObject.TypeNames.Insert(0,"MamlCommandHelpInfo#syntax")
        }
        $cmdSyntax
    }
}

<#
.SYNOPSIS
Set the location
.DESCRIPTION
Proxy function for the built-in Set-Location cmdlet, with a default Path argument of $Home
.PARAMETER Path
The location to change to
.NOTES
Intended to be aliased to 'cd', which by default has no default argument
#>
function Set-MrlLocation {
    [CmdletBinding()] Param(
        $Path = $Home
    )
    Set-Location -Path $Path
}

<#
.SYNOPSIS
Import a module idempotently
.DESCRIPTION
If the module is not already imported, import it.
If the module is already imported, remove it, then import it again from the same path
.PARAMETER Name
The name of the module to (re-)import
#>
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

<#
.SYNOPSIS
Start a batch file
.DESCRIPTION
Start a batch file, preventing a stupid "Terminate batch job? Y/N" prompt if you Ctrl-C the process
Throw if $LASTEXITCODE was nonzero
.PARAMETER BatchFile
The path to the batch file
.PARAMETER BatchArgs
Arguments to pass to the batch file
#>
function Start-BatchFile {
    [CmdletBinding()] Param(
        [parameter(mandatory=$true)] [string] $BatchFile,
        [parameter(ValueFromRemainingArguments=$true)] $BatchArgs
    )
    # we use "<NUL" to prevent that fucking "Terminate batch job? Y/N" prompt
    cmd.exe "/c $BatchFile $($BatchArgs -Join ' ') <NUL"
    if ($LASTEXITCODE -ne 0) {
        throw "Command '$BatchFile $($BatchArgs -Join ' ')' exited with code $LASTEXITCODE"
    }
}

<#
.SYNOPSIS
Fucking extract an archive the right way.
.DESCRIPTION
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
.PARAMETER Archive
A list of archives to extract
.PARAMETER OutDir
The directory to extract the archives to. Defaults to the current working
directory.
.PARAMETER Force
If there is an existing file/directory with the same name as one that would be
extracted, delete the existing item first.
#>
function Expand-FuckingArchive {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] [string[]] $Archive,
        [string] $OutDir,
        [switch] $Force
    )

    <#
    .SYNOPSIS
    Fucking extract an archive to a temporary directory
    .PARAMETER Archive
    An archive file
    .PARAMETER OutDir
    The directory in which to create the temporary extraction dir
    .PARAMETER NoOutDir
    Instead of creating a temporary extraction dir, just use the archive's parent directory
    #>
    function Expand-OneFuckingLayer {
        [CmdletBinding()] param(
            [Parameter(Mandatory=$true)] [System.IO.FileInfo] $Archive,
            [Parameter(Mandatory=$true,ParameterSetName="OutDir")] [System.IO.DirectoryInfo] $OutDir,
            [Parameter(Mandatory=$true,ParameterSetName="NoOutDir")] [switch] $NoOutDir
        )

        if ($NoOutDir) {
            $OutDir = $Archive.Directory.FullName
        } else {
            $tempDirName = "fuckingextract-" + [System.IO.Path]::GetRandomFileName()
            $OutDir = "$($OutDir.FullName)\$tempDirName"
            if (Test-Path -Path $OutDir) {
                throw "The temporary directory that already exists"
            }
            New-Item -Type Directory $OutDir | Out-Null
        }

        $7zcmd = '7z x "-o{0}" "{1}"' -f @($OutDir, $Archive.FullName)
        $7zout = Invoke-Expression -Command $7zcmd
        if ($LASTEXITCODE -ne 0) {
            throw "7z exited with code $LASTEXITCODE`n`tcommand line: $7zcmd`n`toutput: `n$7zout"
        }
        return $OutDir
    }

    try {
        Get-Command -Name 7z | Out-Null
    } catch {
        throw "7z.exe is not in your `$ENV:PATH; cannot continue"
    }

    $secondLayerExtensions = @(".tar") # There aren't any more that I can think of?

    if (-not $OutDir) {
        $OutDir = $pwd
    } elseif (-not (Test-Path -Path $OutDir)) {
        New-Item -Type Directory -Force $OutDir | Out-Null
    }
    $outDirItem = Get-Item -Path $OutDir

    $outFiles = @()
    foreach ($arch in $archive) {
        $archItem = Get-Item -Path $arch

        # this is the name of the archive w/o its extension
        # this will be used as the eventual directory name to extract to
        $archBareName = [System.IO.Path]::GetFileNameWithoutExtension($archItem.Name)

        $exDir = Expand-OneFuckingLayer -Archive $archItem -Outdir $outDirItem
        Write-Verbose -Message "Using temporary extraction directory: $exDir"
        $exItems = Get-ChildItem -Path $exDir

        # If there is exactly one item in the archive which has an extension in
        # $secondLayerExtensions, extract that item too.
        if (
            $exItems.Count -eq 1 -and
            $secondLayerExtensions | Where-Object -FilterScript { $exItems[0].Name.EndsWith($_) }
        ) {
            $innerArchItem = $exItems[0]
            write-verbose "Found inner archive: $($innerArchItem.fullname)"
            Expand-OneFuckingLayer -Archive $innerArchItem.FullName -NoOutDir | Out-Null
            $archBareName = [System.IO.Path]::GetFileNameWithoutExtension($innerArchItem.Name)
            Remove-Item -Path $innerArchItem.FullName
            $exItems = Get-ChildItem -Path $exDir
        }

        if ($exItems.Count -eq 1) {
            $outItem = $exItems[0]
            $outItemName = Join-Path -Path $outDirItem -ChildPath $outItem.Name
            Write-Verbose -Message "Only one item in archive: '$($outItem.FullName)'; moving to '$($outDirItem.FullName)'"

            if ((Test-Path -Path $outItemName) -and $Force) {
                Write-Verbose -Message "Found existing item at '$outItemName' but -force was specified; removing..."
                Remove-Item -Recurse -Force -Path $outItemName
            } elseif ((Test-Path -Path $outItemName) -and -not $Force) {
                throw "Extracted archive to '$exDir' but could not move to '$outItemName' because '$outItemName' already exists"
            }

            $outFiles += @(Move-Item -Path $outItem.FullName -Destination $outItemName -PassThru)
            Remove-Item -Recurse -Path $exDir
        } else {
            $outItemName = Join-Path -Path $outDirItem -ChildPath $archBareName
            Write-Verbose -Message "Multiple items in archive; moving temporary extraction directory to '$outItemName'"

            if ((Test-Path -Path $outItemName) -and $Force) {
                Write-Verbose -Message "Found existing item at '$outItemName' but -force was specified; removing..."
                Remove-Item -Recurse -Force -Path $outItemName
            } elseif ((Test-Path -Path $outItemName) -and -not $Force) {
                throw "Extracted archive to '$exDir' but could not move to '$outItemName' because '$outItemName' already exists"
            }

            $outFiles += @(Move-Item -Path $exDir Destination $outItemName -PassThru)
        }
    }

    return $outFiles
}

<#
.SYNOPSIS
Set the title of a ConEmu tab
.PARAMETER Prefix
A prefix character. Just for fun lol.
.PARAMETER Title
A title
#>
function Set-ConEmuTabTitle {
    [cmdletbinding()] param(
        $Prefix = [char]187,    # » (RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK)
        $Title
    )
    # Calling from cmd.exe fixes some quoting issues
    $out = cmd.exe /c "ConEmuC.exe -Guimacro Rename(0, `"$Prefix $Title`")"
    if ($out -ne "OK") {
        # Unfortunately, ConEmuC does _not_ set a nonzero exit code when the macro fails
        throw "Failed to change tab title with error: $out"
    }
}


<#
.SYNOPSIS
Set the colors for different types of text being displayed in the console
.DESCRIPTION
Useful in case one of those fucking color commands perma fucks with your background/foreground
color. These are my preferences. Get the default values by launching Powershell -NoProfile and
examining $Host.UI.RawUI and $Host.PrivateData
#>
function Set-ConsoleColors {
    [CmdletBinding()] Param(
        $BackgroundColor = 'Black',
        $ForegroundColor = 'White',
        $ErrorForegroundColor = 'Red',
        $ErrorBackgroundColor = 'Black',
        $WarningForegroundColor = 'Magenta',
        $WarningBackgroundColor = 'Black',
        $DebugForegroundColor = 'Yellow',
        $DebugBackgroundColor = 'Black',
        $VerboseForegroundColor = 'Green',
        $VerboseBackgroundColor = 'Black',
        $ProgressForegroundColor = 'DarkBlue',
        $ProgressBackgroundColor = 'White'
    )

    function ConditionallySetProperty {
        Param(
            [Parameter(Mandatory)] $object,
            [Parameter(Mandatory)] $propertyName,
            [Parameter(Mandatory)] $propertyValue
        )
        if (($object | Get-Member | Select-Object -ExpandProperty Name) -Contains $propertyName) {
            $object.$propertyName = $propertyValue
        }
    }

    [Console]::ResetColor()

    $Host.UI.RawUI.BackgroundColor = $BackgroundColor
    $Host.UI.RawUI.ForegroundColor = $ForegroundColor

    ConditionallySetProperty -object $Host.PrivateData -propertyName ErrorForegroundColor -propertyValue $ErrorForegroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName ErrorBackgroundColor -propertyValue $ErrorBackgroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName WarningForegroundColor -propertyValue $WarningForegroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName WarningBackgroundColor -propertyValue $WarningBackgroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName DebugForegroundColor -propertyValue $DebugForegroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName DebugBackgroundColor -propertyValue $DebugBackgroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName VerboseForegroundColor -propertyValue $VerboseForegroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName VerboseBackgroundColor -propertyValue $VerboseBackgroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName ProgressForegroundColor -propertyValue $ProgressForegroundColor
    ConditionallySetProperty -object $Host.PrivateData -propertyName ProgressBackgroundColor -propertyValue $ProgressBackgroundColor
}
