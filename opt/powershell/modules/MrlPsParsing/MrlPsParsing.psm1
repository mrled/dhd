<#
.DESCRIPTION
Functions related to Powershell parsing, the AST, etc.
#>

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


<#
.SYNOPSIS
Invoke the Powershell parser and return the AST
.PARAMETER Text
A text string containing Powershell code
.PARAMETER Path
A path to a file containing Powershell code
.OUTPUTS
An object with properties:
- Path:     The path to the file if -Path was passed
- Code:     The code passed as -Text or the contents of the file passed as -Path
- AST:      The AST result from the parse function
- Tokens:   All tokens from the parse function
- Errors:   All errors from the parse function
#>
function Invoke-PowershellParser {
    [CmdletBinding(DefaultParameterSetName='FromFile')]
    Param(
        [Parameter(Mandatory, Position=0, ValueFromPipeline=$true, ParameterSetName='FromText')] [string] $Text,
        [Parameter(Mandatory, Position=0, ParameterSetName='FromFile')] [string] $Path
    )

    $tokens = @()
    $parseErrors = @()

    switch ($PsCmdlet.ParameterSetName) {
        'FromText' {
            $parsed = [Management.Automation.Language.Parser]::ParseInput($Text, [ref]$tokens, [ref]$parseErrors)
        }
        'FromFile' {
            $Text = Get-Content -Path $Path
            $Path = Resolve-Path -Path $Path | Select-Object -ExpandProperty Path
            $parsed = [Management.Automation.Language.Parser]::ParseFile($Path, [ref]$tokens, [ref]$parseErrors)
        }
        default {
            throw "Unknown parameter set '$($PsCmdlet.ParameterSetName)'"
        }
    }

    Write-Verbose -Message "$($tokens.Count) tokens found."
    Write-Verbose -Message "$($parseErrors.Count) errors found."

    return New-Object -TypeName PSObject -Property @{
        Path = $Path
        Code = $Text
        AST = $parsed
        Tokens = $tokens
        Errors = $parseErrors
    }
}

<#
.SYNOPSIS
Test for valid Powershell syntax
.PARAMETER Text
A text string containing Powershell code
.PARAMETER Path
A path to a file containing Powershell code
.PARAMETER ThrowOnFailure
If true, throw if the Powershell code is not valid
.OUTPUTS
Returns True if the code is valid, or False if it is not
#>
function Test-PowershellSyntax {
    [CmdletBinding(DefaultParameterSetName='FromFile')]
    Param(
        [Parameter(Mandatory, Position=0, ValueFromPipeline=$true, ParameterSetName='FromText')] [string] $Text,
        [Parameter(Mandatory, Position=0, ParameterSetName='FromFile')] [string] $Path,
        [switch] $ThrowOnFailure
    )
    $PSBoundParameters.Remove("ThrowOnFailure") | Out-Null
    $parsed = Invoke-PowershellParser @PSBoundParameters
    if ($parsed.Errors.Count -gt 0) {
        $message = "Found $($parsed.Errors.Count) parsing errors:`r`n"
        $ctr = 0
        foreach ($parseErr in $parsed.Errors) {
            $message += "`r`nError #${ctr}:`r`n$parseErr"
            $ctr++
        }
        if ($ThrowOnFailure) {
            throw $message
        }
        Write-Verbose -Message $message
        return $false
    } else {
        return $true
    }
}

<#
.SYNOPSIS
Get AST filters defined in this module
.PARAMETER Name
If passed, return only a filter with this exact name
#>
function Get-AstFilter {
    [CmdletBinding()] Param(
        [string] $Name
    )
    if ($Name) {
        if (-not $AstFilters.ContainsKey($Name)) {
            throw "No such filter '$Name'"
        }
        return $AstFilters[$Name]
    } else {
        return $AstFilters
    }
}

<#
.SYNOPSIS
Find objects in the Powershell AST
.PARAMETER FilterName
The name of an existing filter defined in this module (see Get-AstFilter)
.PARAMETER Filter
A Scriptblock filter for AST objects, which will be passed to the AST object's .FindAll() method
.PARAMETER Ast
A Powershell AST
.PARAMETER RecurseNested
If passed, look inside nested scope; otherwise, look only at root level.
#>
function Find-AstObjects {
    [CmdletBinding(DefaultParameterSetName="Predefined")] Param(
        [Parameter(Mandatory, Position=0, ParameterSetName="Predefined")] [string] $FilterName,
        [Parameter(Mandatory, Position=0, ParameterSetName="Provided")] [ScriptBlock] $Filter,
        [Parameter(Mandatory, Position=1)] [System.Management.Automation.Language.Ast] $Ast,
        [switch] $RecurseNested
    )
    if ($PsCmdlet.ParameterSetName -eq "Predefined") {
        $Filter = Get-AstFilter -Name $FilterName
    }
    return $Ast.FindAll($Filter, $RecurseNested)
}


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

    [bool] EqualsEx(
        [QualifiedCommand] $Other
    ) {
        return $this.Identity() -eq $Other.Identity()
    }

}

function Select-UniqueQualifiedCommand {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [QualifiedCommand[]] $QualifiedCommand
    )
    $seen = @()
    foreach ($cmd in $QualifiedCommand) {
        if ($cmd.Identity() -In $seen) {
            continue
        }
        $seen += $cmd.Identity()
        $cmd
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
    [bool] EqualsEx(
        [FunctionCallEdge] $Other
    ) {
        return $this.Source.EqualsEx($Other.Source) -And $this.Target.EqualsEx($Other.Target)
    }
    [string] Identity() {
        return "{0}->{1}" -f $this.Source.Identity(), $this.Target.Identity()
    }
}

function Select-UniqueFunctionCallEdge {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [FunctionCallEdge[]] $FunctionCallEdge
    )
    $seen = @()
    foreach ($edge in $FunctionCallEdge) {
        if ($edge.Identity() -In $seen) {
            continue
        }
        $seen += $edge.Identity()
        $edge
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

    try {
        $foundCmds = Get-Command -Name $Name
    } catch {
        Write-Verbose -Message "Could not find function named '$Name' in any currently-loaded module"
        return $null
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

        # In modules, find defined functions
        # In other files like scripts ending in .ps1,
        # ignore defined functions
        if ($pathItem.Name.EndsWith(".psm1")) {
            # Get each function *DEFINED* at the root of the file
            # (i.e. exclude nested functions, defined within another function)
            # and save their AST to $functionAstMap
            foreach ($definedFuncAst in (Find-AstObjects -FilterName DefinedFunctions -Ast $parsedFile.AST)) {
                $qualifiedFunc = New-Object -TypeName QualifiedCommand -ArgumentList @($pathItem, $definedFuncAst.Name, $False)
                $functionAstMap.Add($qualifiedFunc, $definedFuncAst)
            }
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
        # $recurseNested = -Not $function.ScriptRoot

        # If we are in a script, look inside any nested scopes to find function calls
        # If we are in a module at root scope, do NOT look inside nested scopes for function calls
        # If we are in a module at function scope, DO look inside nested scopes for function calls
        $recurseNested = $function.File.Name.EndsWith("ps1") -Or -Not $function.ScriptRoot

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
