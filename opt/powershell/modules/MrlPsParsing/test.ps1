<#
.SYNOPSIS
Test generating a graph of function calls in an extracted Ed-Fi ODS database deployment package
#>
[CmdletBinding()] Param(
    $DotPath = "$Home\Downloads\FunctionGraph.dot",
    $ImagePath = "$Home\Downloads\FunctionGraph.pdf",
    $ImageFormat = "pdf",
    $RootScriptPath = (Resolve-Path -Path "$Home\Downloads\edfidb\EdFi.RestApi.Databases.0.0.49-bps" | Select-Object -ExpandProperty Path),
    $RootScriptPsDriveLetter = "V",
    $ExcludeSourceFilePattern = @(
        "DeployDatabasesToAzure\.ps1$"
        "\\EntityFramework\\"
        "psake\.psm1$"
        "\.credentials\.ps1$"
        "\.Tests\.ps1$"
        "\.vars\.ps1$"
        "\-vars\.ps1$"
        '\\sqlps\\'
    )
)

$Error.Clear()
$ErrorActionPreference = "Stop"

Import-Module -Name PSGraph
Import-Module -Name MrlPsParsing

<#
.DESCRIPTION
A command name, along with a FileInfo object pointing to the file that defines the command
#>
class QualifiedCommand {

    # If $File is $Null, that means the file where the command is defined is unknown
    [System.IO.FileInfo] $File;

    # The string name of the command. Null if the command represents a script file itself
    [string] $Command;

    # True if the command represents a script file itself; false otherwise
    [bool] $ScriptRoot;

    hidden [string[]] $ReplaceChars = @('\-', '\.', '\\', '\:')

    QualifiedCommand() {}

    QualifiedCommand(
        [System.IO.FileInfo] $File,
        [string] $Command,
        [bool] $ScriptRoot
    ) {
        $this.File = $File
        if (($Command -And $ScriptRoot) -Or (-Not $Command -And -Not $ScriptRoot)) {
            throw "Command must be a string and ScriptRoot false, or Command must be null and ScriptRoot true"
        }
        $this.Command = $Command
        $this.ScriptRoot = $ScriptRoot
    }

    [string] ToString() {
        return $this.Label()
    }

    [string] FileIdentity() {
        $fileId = $this.File.FullName
        foreach ($char in $this.ReplaceChars) {
            $fileId = $fileId -Replace $char, '_'
        }
        return $fileId.ToLower()
    }

    [string] FileLabel() {
        return $this.File.FullName -Replace "\\", "\\"
    }

    [string] CommandIdentity() {
        if ($this.ScriptRoot) {
            $cmdId = "ROOT"
        } else {
            $cmdId = $this.Command
        }
        foreach ($char in $this.ReplaceChars) {
            $cmdId = $cmdId -Replace $char, '_'
        }
        return $cmdId.ToLower()
    }

    [string] CommandLabel() {
        if ($this.ScriptRoot) {
            return "ROOT"
        } else {
            return $this.Command
        }
    }

    [string] Identity() {
        return "{0}:{1}" -f $this.FileIdentity(), $this.CommandIdentity()
    }

    [string] Label() {
        return "{0}:{1}" -f $this.FileLabel(), $this.CommandLabel()
    }

}

<#
.DESCRIPTION
A graph edge, connecting two QualifiedCommand instances
#>
class FunctionCallEdge {
    [QualifiedCommand] $Source;
    [QualifiedCommand] $Target;
    FunctionCallEdge(
        [QualifiedCommand] $Source,
        [QualifiedCommand] $Target
    ) {
        $this.Source = $Source
        $this.Target = $Target
    }
}

<#
.SYNOPSIS
Find the file where a command is defined
.PARAMETER Name
The name of a command to search for
.PARAMETER AliasRecurseLength
How many times will we attempt to resolve an alias before giving up and assuming it's circular
.OUTPUTS
If it can find the location that a command is defined,
return a FileInfo object for that location;
otherwise, return $Null.
#>
function Resolve-CommandDefinitionFile {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name,
        [int] $AliasRecurseLength
    )

    # Dumb hacks o'clock
    switch ($Name) {
        '?' { $Name = 'Where-Object' }
    }

    $foundCmds = $script:allCommands | Where-Object -Property Name -eq $Name
    if (-not $foundCmds) {
        Write-Verbose -Message "Could not find function named '$Name' in any currently-loaded module"
        return $Null
    }

    # There may be more than one result! For instance, BoxStarter defines its own Write-Host alias (lol)
    foreach ($command in $foundCmds) {

        $ctr = 0
        while ($command.CommandType -eq [System.Management.Automation.CommandTypes]::Alias) {
            if ($ctr -gt $AliasRecurseLength) {
                throw "There's a chain of aliases greater than $AliasRecurseLength long; aborting..."
            }
            $command = $command.ResolvedCommand
        }

        try {
            if (-not $command.CommandType) {
                Write-Warning -Message "Having trouble resolving '$Name', ignoring..."
                continue
            } elseif ($command.CommandType -eq [System.Management.Automation.CommandTypes]::Function) {
                return $Null
            } elseif ($command.CommandType -in @( [System.Management.Automation.CommandTypes]::Application, [System.Management.Automation.CommandTypes]::ExternalScript ) ) {
                return Get-Item -Path $command.Source
            } elseif ($command.CommandType -eq [System.Management.Automation.CommandTypes]::Cmdlet) {
                # You'd think you can just do this:
                #   return Get-Item -Path $command.DLL
                # Naturally, that doesn't work; the .DLL property somehow is not populated sometimes,
                # even though doing Get-Command on the problematic command in the terminal has a .DLL property
                # I have seen this with at least Start-Transcript;
                # most other commands don't seem to exhibit this behavior
                # smmfgdh
                return Get-Item "$PSHome\powershell.exe"
            }
        } catch {
            Write-Warning -Message "Got an error after resolving '$Name' to '$($command.Name)'"
            throw $_
        }
        throw "No support for commands of type '$($command.CommandType)' (trying to resolve command '$Name')"
    }
}

<#
.SYNOPSIS
Test whether an input string matches any pattern in a list
.PARAMETER String
An input string
.PARAMETER PatternList
A list of regex patterns to test
#>
function Test-MatchesAnyPattern {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $String,
        [Parameter(Mandatory)] [string[]] $PatternList
    )
    foreach ($pattern in $PatternList) {
        if ($String -Match $pattern) {
            return $true
        }
    }
    return $false
}

<#
.SYNOPSIS
Get an AST for all functions defined and called in a Powershell file
.PARAMETER Path
A list of Powershell files
.OUTPUTS
A .NET dictionary with keys of type QualifiedCommand and values of type System.Management.Automation.Language.Ast
#>
function New-FunctionCallAstMap {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string[]] $Path
    )

    # A .NET dictionary having keys of QualifiedCommand and values of Powershell AST
    $functionAstMap = New-Object -TypeName 'System.Collections.Generic.Dictionary[QualifiedCommand,System.Management.Automation.Language.Ast]'

    foreach ($pathEntry in $Path) {
        $pathItem = Get-Item -Path $pathEntry
        $parsedFile = Invoke-PowershellParser -Path $pathItem.FullName

        # Save the whole file's AST to $functionAstMap
        $rootQualCmd = New-Object -TypeName QualifiedCommand -ArgumentList @($pathItem, $Null, $True)
        $functionAstMap.Add($rootQualCmd, $parsedFile.AST)

        # Get each function *DEFINED* at the root of the file
        # (i.e. exclude nested functions, defined within another function)
        # and save their AST to $functionAstMap
        foreach ($definedFuncAst in (Find-AstObjects -FilterName DefinedFunctions -Ast $parsedFile.AST)) {
            $qualifiedFunc = New-Object -TypeName QualifiedCommand -ArgumentList @($pathItem, $definedFuncAst.Name, $False)
            $functionAstMap.Add($qualifiedFunc, $definedFuncAst)
        }
    }

    return $functionAstMap
}

<#
.SYNOPSIS
Get a graph representation of all functions defined and called from a function-AST map
.PARAMETER FunctionAstMap
A .NET generic dictionary with keys of type QualifiedCommand and values of type System.Management.Automation.Language.Ast
.OUTPUTS
A list of FunctionCallEdge objects
#>
function Get-FunctionCallEdgeList {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)]
        [System.Collections.Generic.Dictionary[QualifiedCommand,System.Management.Automation.Language.Ast]]
        $FunctionAstMap
    )

    # A list of QualifiedCommand objects that we have seen before
    $qualCmds = $FunctionAstMap.Keys

    # List of FunctionCallEdge objects
    $edges = @()

    # $FunctionAstMap keys are fully qualified function names of _defined_ functions
    # Here, we attempt to resolve fully qualified names for _called_ functions,
    # to populate the $edges array
    # (If we cannot resolve a fully qualified name,
    # the .File property of the unresolvable function will remain $Null)
    foreach ($kvp in $FunctionAstMap.GetEnumerator()) {
        $function = $kvp.Key
        $ast = $kvp.Value

        # If the entry represents the root of a script,
        # then the value represents the AST for the code in _the entire file_,
        # naturally including function definitions.
        # If the entry does NOT represent the root of a script,
        # then the value represents the AST for the code _just in that function_.
        # When enumerating called functions,
        # only look inside function definitions in the second case.
        $recurseNested = -Not $function.ScriptRoot

        $calledFuncNames = Find-AstObjects -FilterName CalledFunctions -AST $ast -RecurseNested:$recurseNested |
            Foreach-Object -Process { $_.CommandElements[0].Value } |
            Sort-Object -Unique

        foreach ($calledFunc in $calledFuncNames) {

            # If $qualCmds alrady has an entry for this function,
            # then we saw it earlier when enumerating defined functions.
            # Otherwise, try to make a reasonable guess about the source of the function.
            $existingQualCmds = $qualCmds | Where-Object -Property Command -EQ $calledFunc
            if (-Not ($existingQualCmds)) {
                # NOTE: Resolve-CommandDefinitionFile may return an array if there are multiple places a command is defined
                $existingQualCmds = @()
                foreach ($cmdDefFile in (Resolve-CommandDefinitionFile -Name $calledFunc)) {
                    $existingQualCmds += New-Object -TypeName QualifiedCommand -ArgumentList @($cmdDefFile, $calledFunc, $False)
                }
                $qualCmds += $existingQualCmds
            }

            foreach ($existingQualCmd in $existingQualCmds) {
                $edges += New-Object -TypeName FunctionCallEdge -ArgumentList @($function, $existingQualCmd)
            }
        }
    }

    return $edges
}

<#
.SYNOPSIS
Generate a function call graph from its list of edges
.NOTES
We use a bidirectional graph which is according to Rafferty the correct type to use when graphing dependencies
#>
function New-FunctionCallGraph {
    [CmdletBinding()] Param(
        [Parameter(Mandatory, ValueFromPipeline=$true)]
        [FunctionCallEdge[]]
        $EdgeList
    )

    $excludeTargets = @(
        'powershell.exe'
        'psake.psm1'
    )

    $allNodes = $EdgeList |
        Foreach-Object -Process { @($_.Source, $_.Target) } |
        Select-Object -Unique

    $allFiles = $allNodes |
        Select-Object -ExpandProperty File -Unique

    $functionsByFile = @{}
    foreach ($uniqueFile in $allFiles) {
        $functionsByFile[$uniqueFile.FullName] = $allNodes |
            Where-Object -FilterScript { $_.File.FullName -eq $uniqueFile.FullName }
    }
    function LabelMaker([QualifiedCommand[]] $Commands) {
        $output = @(
            "<{0}>{1}" -f $Commands[0].FileIdentity(), $Commands[0].FileLabel()
        )
        foreach ($cmd in $Commands) {
            $output += "<{0}>{1}" -f $cmd.CommandIdentity(), $cmd.CommandLabel()
        }
        return $output -Join "|"
    }

    $graphParams = @{
        Name = "FunctionCallGraph"
        Attributes = @{
            label = "Function Call Graph"
            overlap = $False
            splines = $True
            rankdir = 'LR'
            concentrate = $True
        }
        ScriptBlock = {
            foreach ($filePath in $functionsByFile.Keys) {
                $fileInfo = Get-Item -Path $filePath
                if ($fileInfo.Name -In $excludeTargets) {
                    continue
                }
                $fileId = $functionsByFile[$filePath][0].FileIdentity()
                Node -Name $fileId -Attributes @{
                    shape = 'record'
                    label = LabelMaker -Commands $functionsByFile[$filePath]
                }
            }
            foreach ($edge in $EdgeList) {
                if ($edge.Target.File.Name -in $excludeTargets) {
                    continue
                }
                Edge -From $edge.Source.Identity() -To $edge.Target.Identity()
            }
        }
    }
    $graph = Graph @graphParams

    return $graph
}

if (-not $allCommands) {
    Write-Host -ForegroundColor Green -Object "Caching output of 'Get-Command'... " -NoNewLine
    $allCommands = Get-Command
    Write-Host -ForegroundColor Green -Object "Done"
}

if (-Not (Get-PSDrive | Where-Object -Property Name -EQ -Value $RootScriptPsDrive)) {
    $RootScriptPath = Resolve-Path -Path $RootScriptPath | Select-Object -ExpandProperty Path
    $RootScriptPath = $RootScriptPath -Replace "\\$", ""
    subst.exe "${RootScriptPsDriveLetter}:" /D
    subst.exe "${RootScriptPsDriveLetter}:" "$RootScriptPath"
    Get-PSDrive | Out-Null
}

Write-Host -ForegroundColor Green -Object "Retrieving sample Powershell file list... " -NoNewLine
$allPsUnfiltered = Get-ChildItem -Recurse -Include "*.ps1","*.psm1" -Path "${RootScriptPsDriveLetter}:"
$allPs = @()
foreach ($psFile in $allPsUnfiltered) {
    if (Test-MatchesAnyPattern -String $psFile.FullName -PatternList $ExcludeSourceFilePattern) {
        $allPs += $psFile
    }
}
Write-Host -ForegroundColor Green -Object "Done"

Write-Host -ForegroundColor Green -Object "Building function call / AST map... " -NoNewLine
$functionAstMap = New-FunctionCallAstMap -Path $allPs
Write-Host -ForegroundColor Green -Object "Done"

Write-Host -ForegroundColor Green -Object "Building list of edges... " -NoNewLine
$edges = Get-FunctionCallEdgeList -FunctionAstMap $functionAstMap
Write-Host -ForegroundColor Green -Object "Done"

Write-Host -ForegroundColor Green -Object "Building graph... " -NoNewLine
$graph = New-FunctionCallGraph -EdgeList $edges
Out-File -FilePath $DotPath -Encoding ASCII -Force -InputObject $graph
Write-Host -ForegroundColor Green -Object "Done"

Write-Host -ForegroundColor Green -Object "Building graph -ical representation... " -NoNewLine
Export-PSGraph -Source $DotPath -DestinationPath $ImagePath -ShowGraph -OutputFormat $ImageFormat
Write-Host -ForegroundColor Green -Object "Done"

Start-Process -FilePath $ImagePath

# return $graph
