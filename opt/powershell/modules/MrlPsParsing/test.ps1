[CmdletBinding()] Param(
)

$Error.Clear()
$ErrorActionPreference = "Stop"

Invoke-Command -ScriptBlock {
    Import-Module $PSScriptRoot
    $qgLib = "$PSScriptRoot\Packages\YC.QuickGraph.3.7.3\lib\net45"
    Add-Type -Path @(
        "$qgLib\YC.QuickGraph.dll"
        "$qgLib\YC.QuickGraph.Data.dll"
        "$qgLib\YC.QuickGraph.Glee.dll"
        "$qgLib\YC.QuickGraph.Graphviz.dll"
        "$qgLib\YC.QuickGraph.Petri.dll"
    )
}

$AstFilters = @{
    DefinedFunctions = {
        Param(
            [System.Management.Automation.Language.Ast] $Ast
        )
        $isFunctionDefinitionAst = $Ast -is [System.Management.Automation.Language.FunctionDefinitionAst]
        $noClassSupport = $PSVersionTable.PSVersion.Major -lt 5
        $notChildOfClass = $Ast.Parent -IsNot [System.Management.Automation.Language.FunctionMemberAst]
        return $isFunctionDefinitionAst -and ($noClassSupport -or $notChildOfClass)
    }
    CalledFunctions = {
        Param(
            [System.Management.Automation.Language.Ast] $Ast
        )
        return $Ast -is [System.Management.Automation.Language.CommandAst]
    }
}

class FullyQualifiedFunction {
    # If $File is $Null, that means the file where the function is defined is unknown
    [System.IO.FileInfo] $File;
    # If $Function is $Null, that indicates the file's root scope, outside of any function
    [string] $Function;
    FullyQualifiedFunction() {}
    FullyQualifiedFunction(
        [System.IO.FileInfo] $File,
        [string] $Function
    ) {
        $this.File = $File
        $this.Function = $Function
    }
    [string] ToString() {
        return '{0}\{1}' -f $this.File.Name, $this.Function
    }
}

class FuckingWip2Edge : QuickGraph.IEdge[FullyQualifiedFunction] {
    [FullyQualifiedFunction] $Source;
    [FullyQualifiedFunction] $Target;
    [int] $Weight;
    FuckingWip2Edge(
        [FullyQualifiedFunction] $Source,
        [FullyQualifiedFunction] $Target
        # [int] $Weight
    ) {
        $this.Source = $Source
        $this.Target = $Target
        $this.Weight = 1
    }
    FuckingWip2Edge(
        [System.IO.FileInfo] $Path,
        [string] $SourceFunction,
        [string] $TargetFunction
    ) {
        $this.Source = [FullyQualifiedFunction] @{ File=$Path; Function=$SourceFunction; }
        $this.Target = [FullyQualifiedFunction] @{ File=$Null; Function=$TargetFunction; }
        $this.Weight = 1
    }
    [FullyQualifiedFunction] get_Source() {
        return $this.Source
    }
    [FullyQualifiedFunction] get_Target() {
        return $this.Target
    }
    [int] get_Weight() {
        return $this.Weight
    }
}

function Get-FuckingWip2 {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string[]] $Path
    )
    function Get-CalledFunctionNameList($CalledFunctionAst) {
        return $CalledFunctionAst | Foreach-Object -Process { $_.CommandElements[0].Value } | Sort-Object -Unique
    }

    # Edges for our graph
    $edges = @()

    # A mapping of functions we have found => filenames they are found in
    $funcFileMap = @{}

    foreach ($pathEntryString in $Path) {
        $pathEntry = Get-Item $pathEntryString
        Write-Verbose -Message "Parsing file at '$pathEntry'"
        $parsedFile = Invoke-PowershellParser -Path $pathEntry

        # Get all functions *CALLED* at the root of the file (i.e. not in another function)
        $calledRootFuncs = $parsedFile.AST.FindAll($AstFilters.CalledFunctions, $false)
        Write-Verbose -Message "Found $($calledRootFuncs.Count) functions called from the root of $pathEntry"
        foreach ($calledFunc in @(Get-CalledFunctionNameList $calledRootFuncs)) {
            $edges += New-Object FuckingWip2Edge -ArgumentList @($pathEntry, $Null, $calledFunc)
        }

        # Get each function *DEFINED* at the root of the file
        # (i.e. exclude nested functions, defined within another function)
        # and list all the functions *CALLED* from inside it
        $definedRootFuncs = $parsedFile.AST.FindAll($AstFilters.DefinedFunctions, $false)
        foreach ($drFunc in $definedRootFuncs) {
            $funcFileMap[$drFunc.Name] = $pathEntry
            $parsedFunc = Invoke-PowershellParser -Text $drFunc.Body
            $calledInnerFuncs = $parsedFunc.AST.FindAll($AstFilters.CalledFunctions, $true)
            foreach ($calledFunc in @(Get-CalledFunctionNameList $calledInnerFuncs)) {
                $edges += New-Object FuckingWip2Edge -ArgumentList @($pathEntry, $drFunc.Name, $calledFunc)
            }
        }
    }

    foreach ($edge in $edges) {
        if ($edge.Target.File -eq $Null) {
            if ($funcFileMap.ContainsKey($edge.Target.Function)) {
                $edge.Target.File = $funcFileMap[$edge.Target.Function]
            } else {
                try {
                    $command = Get-Command -Name $edge.Target.Function
                } catch {
                    Write-Verbose -Message "Could not find function named '$($edge.Target.Function)' in any currently-loaded module"
                }
                while ($command) {
                    switch ($command.CommandType) {
                        [System.Management.Automation.CommandTypes]::Alias {
                            $command = $command.ResolvedCommand
                            continue
                        }
                        [System.Management.Automation.CommandTypes]::Application {
                            $edge.Target.File = Get-Item -Path $command.Source
                        }
                        [System.Management.Automation.CommandTypes]::ExternalScript {
                            $edge.Target.File = Get-Item -Path $command.Source
                        }
                        [System.Management.Automation.CommandTypes]::Cmdlet {
                            $edge.Target.File = Get-Item -Path $command.Source
                        }
                        default {
                            throw "Never planned for working with command of type '$($command.CommandType)'"
                        }
                    }

                    if ($command.CommandType -eq [System.Management.Automation.CommandTypes]::Alias) {

                    } else {
                        $edge.Target.File = Get-Item -Path $command.Module.Path
                        $command = $Null
                    }
                }
            }
        }
    }

    $graph = New-Object -TypeName 'QuickGraph.AdjacencyGraph[FullyQualifiedFunction,FuckingWip2Edge]'
    foreach ($edge in $edges) {
        $graph.AddVerticesAndEdge($edge) | Out-Null
    }

    $gvAlgo = New-Object -TypeName 'QuickGraph.Graphviz.GraphvizAlgorithm[FullyQualifiedFunction,FuckingWip2Edge]' -ArgumentList @($graph)
    $gvAlgo.Generate((New-Object -TypeName QuickGraph.Graphviz.FileDotEngine), "$PSScriptRoot\output.dot")

    return $graph
}

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

        if (-not $command.CommandType) {
            Write-Warning -Message "Having trouble resolving '$Name', ignoring..."
        } elseif ($command.CommandType -eq [System.Management.Automation.CommandTypes]::Function) {
            return $Null
        } elseif ($command.CommandType -in @( [System.Management.Automation.CommandTypes]::Application, [System.Management.Automation.CommandTypes]::ExternalScript ) ) {
            return Get-Item -Path $command.Source
        } elseif ($command.CommandType -eq [System.Management.Automation.CommandTypes]::Cmdlet) {
            return Get-Item -Path $command.DLL
        } else {
            throw "No support for commands of type '$($command.CommandType)' (trying to resolve command '$Name')"
        }

    }
}


class FuckingWip3Edge : QuickGraph.IEdge[FullyQualifiedFunction] {
    [FullyQualifiedFunction] $Source;
    [FullyQualifiedFunction] $Target;
    [int] $Weight;
    FuckingWip3Edge(
        [FullyQualifiedFunction] $Source,
        [FullyQualifiedFunction] $Target
        # [int] $Weight
    ) {
        $this.Source = $Source
        $this.Target = $Target
        $this.Weight = 1
    }
    [FullyQualifiedFunction] get_Source() {
        return $this.Source
    }
    [FullyQualifiedFunction] get_Target() {
        return $this.Target
    }
    [int] get_Weight() {
        return $this.Weight
    }
}

function Get-FuckingWip3 {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string[]] $Path
    )

    # A .NET dictionary having keys of FullyQualifiedFunction and values of Powershell AST
    $functionAstMap = New-Object -TypeName 'System.Collections.Generic.Dictionary[FullyQualifiedFunction,System.Management.Automation.Language.Ast]'

    # Hashtable having keys of function name and values of FullyQualifiedFunction objects
    $functionFqfMap = @{}

    foreach ($pathEntry in $Path) {
        $pathItem = Get-Item -Path $pathEntry
        $parsedFile = Invoke-PowershellParser -Path $pathItem.FullName

        # Save the whole file's AST to $functionAstMap
        $rootKey = New-Object -TypeName FullyQualifiedFunction -ArgumentList @($pathItem, $Null)
        $functionAstMap.Add($rootKey, $parsedFile.AST)

        # Get each function *DEFINED* at the root of the file
        # (i.e. exclude nested functions, defined within another function)
        # and save their AST to $functionAstMap
        foreach ($definedFuncAst in $parsedFile.AST.FindAll($AstFilters.DefinedFunctions, $false)) {
            $qualifiedFunc = New-Object -TypeName FullyQualifiedFunction -ArgumentList @($pathItem, $definedFuncAst.Name)
            $functionAstMap.Add($qualifiedFunc, $definedFuncAst)
            if (-not $functionFqfMap.ContainsKey($definedFuncAst.Name)) {
                $functionFqfMap.Add($definedFuncAst.Name, @($qualifiedFunc))
            } else {
                # $msg = @(
                #     "Attempted to add function that already exists: $($definedFuncAst.Name)"
                #     "Existing: $($qualifiedFunc.File.FullName)"
                #     "Current:  $($functionFqfMap[$definedFuncAst.Name].File.FullName)"
                # ) -Join "`r`n"
                # Write-Warning -Message $msg
                $functionFqfMap[$definedFuncAst.Name] += $qualifiedFunc
            }
        }
    }

    # Edge definition where each item is a pair of FullyQualifiedFunction instances
    # $functionCallMap = New-Object -TypeName 'System.Collections.Generic.Dictionary[FullyQualifiedFunction,System.Management.Automation.Language.Ast]'
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

        $calledFuncNames = $kvp.Value.FindAll($AstFilters.CalledFunctions, $recurseNested) |
            Foreach-Object -Process { $_.CommandElements[0].Value } |
            Sort-Object -Unique

        foreach ($calledFunc in $calledFuncNames) {
            #XXX
            if (-not $functionFqfMap.ContainsKey($calledFunc)) {
                # $functionFqfMap[$calledFunc] = Resolve-CommandDefinitionFile -Name $calledFunc
                foreach ($cmdDefFile in (Resolve-CommandDefinitionFile -Name $calledFunc)) {
                    $fqFunc = New-Object -TypeName FullyQualifiedFunction -ArgumentList @($cmdDefFile, $calledFunc)
                }
            }

            # $calledFuncQual may be an array...
            # so we have to add a new edge for each
            foreach ($func in $functionFqfMap[$calledFunc]) {
                $edges += New-Object -TypeName FuckingWip3Edge -ArgumentList @($kvp.Key, $func)
            }
        }
    }

    $graph = New-Object -TypeName 'QuickGraph.BidirectionalGraph[FullyQualifiedFunction,FuckingWip3Edge]'
    foreach ($edge in $edges) {
        $graph.AddVerticesAndEdge($edge) | Out-Null
    }

    $gvAlgo = New-Object -TypeName 'QuickGraph.Graphviz.GraphvizAlgorithm[FullyQualifiedFunction,FuckingWip3Edge]' -ArgumentList @($graph)
    $gvAlgo.Generate((New-Object -TypeName QuickGraph.Graphviz.FileDotEngine), "$PSScriptRoot\output.dot")

    return $graph
}

if (-not $allCommands) {
    $allCommands = Get-Command
}
if (-not $allPs) {
    $allPs = Get-ChildItem -Recurse -Include "*.ps1","*.psm1" -Exclude "EntityFramework.psm1","DeployDatabasesToAzure.ps1" -Path C:\Users\mledbetter\Downloads\edfidb\
}
$graph = Get-FuckingWip3 -Path $allPs
$graph


<# NEW PLAN

- Set empty hashtable $definedfuncs=@{} (vertices)
- Loop thru all files
- Find all defined functions
- Set $definedfuncs[filename + functionname] = function AST
- now I have a hashtable of all defined functions w/ their AST
- loop thru each one to get the edges

OBJECT REFERENCE

- quickgraph assumes that two vertices are the same ONLY if they reference the same object in memroy
- the above method should help make sure that happens properly

GRAPH TYPE

- rafferty says the correct graph type for dependency information is a bidirectional graph so use that

ON DECK

- Erroring out on line 305... investigate building the actual graph
- In doing so, take rafferty's suggestion of trying to pass vertices + edges simultaneously

#>