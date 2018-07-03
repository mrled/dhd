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
