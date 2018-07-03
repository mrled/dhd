<#
.SYNOPSIS
Test generating a graph of function calls in an extracted Ed-Fi ODS database deployment package
#>
[CmdletBinding()] Param(
)

$Error.Clear()
$ErrorActionPreference = "Stop"

<#
.DESCRIPTION
A command name, along with a FileInfo object pointing to the file that defines the command
#>
class QualifiedCommand {

    # If $File is $Null, that means the file where the command is defined is unknown
    [System.IO.FileInfo] $File;

    # If $Command is $Null, that indicates the file's root scope, outside of any function
    [string] $Command;

    QualifiedCommand() {}

    QualifiedCommand(
        [System.IO.FileInfo] $File,
        [string] $Command
    ) {
        $this.File = $File
        $this.Command = $Command
    }

    [string] ToString() {
        return '{0}\{1}' -f $this.File.Name, $this.Command
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
.DESCRIPTION
A QuickGraph edge, connecting two QualifiedCommand instances
#>
class FunctionCallEdge : QuickGraph.IEdge[QualifiedCommand] {
    [QualifiedCommand] $Source;
    [QualifiedCommand] $Target;
    [int] $Weight;
    FunctionCallEdge(
        [QualifiedCommand] $Source,
        [QualifiedCommand] $Target
        # [int] $Weight
    ) {
        $this.Source = $Source
        $this.Target = $Target
        $this.Weight = 1
    }
    [QualifiedCommand] get_Source() {
        return $this.Source
    }
    [QualifiedCommand] get_Target() {
        return $this.Target
    }
    [int] get_Weight() {
        return $this.Weight
    }
}

<#
.SYNOPSIS
Get a graph representation of all functions defined and called in a (set of) Powershell file(s)
.PARAMETER Path
A list of Powershell files
.OUTPUTS
A list of FunctionCallEdge objects
#>
function Get-FunctionCallEdgeList {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string[]] $Path
    )

    # A .NET dictionary having keys of QualifiedCommand and values of Powershell AST
    $functionAstMap = New-Object -TypeName 'System.Collections.Generic.Dictionary[QualifiedCommand,System.Management.Automation.Language.Ast]'

    # Hashtable having keys of function name and values of QualifiedCommand objects
    $functionQfMap = @{}

    foreach ($pathEntry in $Path) {
        $pathItem = Get-Item -Path $pathEntry
        $parsedFile = Invoke-PowershellParser -Path $pathItem.FullName

        # Save the whole file's AST to $functionAstMap
        $rootKey = New-Object -TypeName QualifiedCommand -ArgumentList @($pathItem, $Null)
        $functionAstMap.Add($rootKey, $parsedFile.AST)

        # Get each function *DEFINED* at the root of the file
        # (i.e. exclude nested functions, defined within another function)
        # and save their AST to $functionAstMap
        foreach ($definedFuncAst in (Find-AstObjects -FilterName DefinedFunctions -Ast $parsedFile.AST)) {
            $qualifiedFunc = New-Object -TypeName QualifiedCommand -ArgumentList @($pathItem, $definedFuncAst.Name)
            $functionAstMap.Add($qualifiedFunc, $definedFuncAst)
            if (-not $functionQfMap.ContainsKey($definedFuncAst.Name)) {
                $functionQfMap.Add($definedFuncAst.Name, @($qualifiedFunc))
            } else {
                $functionQfMap[$definedFuncAst.Name] += @($qualifiedFunc)
            }
        }
    }

    # List of FunctionCallEdge objects
    $edges = @()

    # $functionAstMap keys are fully qualified function names of _defined_ functions
    # Here, we attempt to resolve fully qualified names for _called_ functions,
    # to populate the $edges array
    # (If we cannot resolve a fully qualified name,
    # the .File property of the unresolvable function will remain $Null)
    foreach ($kvp in $functionAstMap.GetEnumerator()) {

        # If $kvp.Key.Function is $Null,
        # then the value represents the AST for the code in _the entire file_,
        # naturally including function definitions.
        # If it is a string, then the value represents the AST for the code _just in that function_.
        # When enumerating called functions,
        # only look inside function definitions in the second case.
        $recurseNested = $kvp.Key.Function -ne $Null

        $calledFuncNames = Find-AstObjects -FilterName CalledFunctions -AST $kvp.Value |
            Foreach-Object -Process { $_.CommandElements[0].Value } |
            Sort-Object -Unique

        foreach ($calledFunc in $calledFuncNames) {

            # NOTE: Resolve-CommandDefinitionFile may return an array if there are multiple places a command is defined
            # NOTE: QuickGraph only assumes two edges are the same if they point to the same object in memory
            if (-not $functionQfMap.ContainsKey($calledFunc)) {
                foreach ($cmdDefFile in (Resolve-CommandDefinitionFile -Name $calledFunc)) {
                    $functionQfMap[$calledFunc] = New-Object -TypeName QualifiedCommand -ArgumentList @($cmdDefFile, $calledFunc)
                }
            }

            foreach ($func in $functionQfMap[$calledFunc]) {
                $edges += New-Object -TypeName FunctionCallEdge -ArgumentList @($kvp.Key, $func)
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
    $graph = New-Object -TypeName 'QuickGraph.BidirectionalGraph[QualifiedCommand,FunctionCallEdge]'
    foreach ($edge in $EdgeList) {
        $graph.AddVerticesAndEdge($edge) | Out-Null
    }
    return $graph
}

<#
.SYNOPSIS
Generate a .dot file from a bidirectional graph
#>
function New-FunctionCallGraphDot {
    [CmdletBinding()] Param(
        [Parameter(Mandatory, ValueFromPipeline=$true)]
        [QuickGraph.BidirectionalGraph[QualifiedCommand,FunctionCallEdge]] $Graph,

        [Parameter(Mandatory)]
        [string] $OutputFile,

        [ScriptBlock] $VertexFormatter
    )

    $OutputFile = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("$OutputFile")
    $gvAlgo = New-Object -TypeName 'QuickGraph.Graphviz.GraphvizAlgorithm[QualifiedCommand,FunctionCallEdge]' -ArgumentList @($graph)
    if ($VertexFormatter) {
        $gvAlgo.FormatVertex += $VertexFormatter
    }
    $gvAlgo.Generate((New-Object -TypeName QuickGraph.Graphviz.FileDotEngine), $OutputFile)
}

<#
.DESCRIPTION
A format event handler for vertices
#>
$FormatVertex = {
    Parameter(
        [Parameter(Mandatory)] $Sender,
        [Parameter(Mandatory)] [QuickGraph.Graphviz.FormatVertexEventArgs[QualifiedCommand]] $EventArgs
    )
    $EventArgs.VertextFormatter.Label = $EventArgs.Vertex.ToString()
    $EventArgs.VertexFormatter.Shape = [QuickGraph.Graphviz.Dot.GraphvizVertexShape]::PlainText
}

if (-not $allCommands) {
    Write-Host -ForegroundColor Green -Object "Caching output of 'Get-Command'... " -NoNewLine
    $allCommands = Get-Command
    Write-Host -ForegroundColor Green -Object "Done"
}

if (-not $allPs) {
    Write-Host -ForegroundColor Green -Object "Retrieving sample Powershell file list... " -NoNewLine
    $allPs = Get-ChildItem -Recurse -Include "*.ps1","*.psm1" -Exclude "EntityFramework.psm1","DeployDatabasesToAzure.ps1" -Path C:\Users\mledbetter\Downloads\edfidb\
    Write-Host -ForegroundColor Green -Object "Done"
}

Write-Host -ForegroundColor Green -Object "Building list of edges... " -NoNewLine
$edges = Get-FunctionCallEdgeList -Path $allPs
Write-Host -ForegroundColor Green -Object "Done"

Write-Host -ForegroundColor Green -Object "Building graph... " -NoNewLine
$graph = New-FunctionCallGraph -EdgeList $edges
Write-Host -ForegroundColor Green -Object "Done"

Write-Host -ForegroundColor Green -Object "Building graph -ical representation... " -NoNewLine
New-FunctionCallGraphDot -Graph $graph -OutputFile $PSScriptRoot\output.dot -VertexFormatter $FormatVertex
Write-Host -ForegroundColor Green -Object "Done"

return $graph
