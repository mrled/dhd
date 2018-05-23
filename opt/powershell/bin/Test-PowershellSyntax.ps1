<#
.SYNOPSIS
Test for valid Powershell syntax
.PARAMETER Text
A text string containing Powershell code to test
.PARAMETER Path
A path to a file containing Powershell code to test
.PARAMETER ThrowOnFailure
If true, throw if the Powershell code is not valid
.OUTPUTS
Returns True if the code is valid, or False if it is not
#>
[cmdletbinding(DefaultParameterSetName='FromFile')]
param(
    [parameter(mandatory=$true, Position=0, ValueFromPipeline=$true, ParameterSetName='FromText')] [string] $Text,
    [parameter(mandatory=$true, Position=0, ParameterSetName='FromFile')] [string] $Path,
    [switch] $ThrowOnFailure
)
$tokens = @()
$parseErrors = @()
$parser = [Management.Automation.Language.Parser]
switch ($PsCmdlet.ParameterSetName) {
    'FromText' {
        $parsed = $parser::ParseInput($Text, [ref]$tokens, [ref]$parseErrors)
    }
    'FromFile' {
        $Path = Resolve-Path -Path $Path | Select-Object -ExpandProperty Path
        $parsed = $parser::ParseFile($Path, [ref]$tokens, [ref]$parseErrors)
    }
    default {
        throw "Unknown parameter set '$($PsCmdlet.ParameterSetName)'"
    }
}
Write-Verbose -Message "$($tokens.Count) tokens found."

if ($parseErrors.Count -gt 0) {
    $message = "Found $($parseErrors.Count) parsing errors:`r`n"
    foreach ($parseErr in $parseErrors) {
        $message += "`r`n    $_"
    }
    if ($ThrowOnFailure) {
        throw $message
    } else {
        Write-Verbose -Message $message
    }
    return $false
} else {
    return $true
}
