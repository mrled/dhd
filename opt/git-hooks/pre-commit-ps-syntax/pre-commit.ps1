# Powershell Pre-Commit Hook

### Settings

#$psExtensions = @('ps1','psm1','psd1')
$psExtensions = 'ps1|psm1|psd1'

### Functions

function Test-PowershellSyntax {
    [cmdletbinding(DefaultParameterSetName='FromText')]
    param(
        [parameter(mandatory=$true,ParameterSetName='FromText')] [string] $text,
        [parameter(mandatory=$true,ParameterSetName='FromFile')] [string] $fileName
    )
    $tokens = @()
    $parseErrors = @()
    if ($pscmdlet.ParameterSetName -eq 'FromText') {
        $parsed = [System.Management.Automation.Language.Parser]::ParseInput(
            $text, [ref]$tokens, [ref]$parseErrors)
    }
    elseif ($pscmdlet.ParameterSetName -eq 'FromFile') {
        $fileName = resolve-path $fileName
        $parsed = [System.Management.Automation.Language.Parser]::ParseFile(
            $fileName, [ref]$tokens, [ref]$parseErrors)
    }

    if ($parseErrors.count -gt 0) {
        write-host -foreground red "$($parseErrors.count) parse errors found."
        foreach ($e in $parseErrors) {
            write-host -foreground red "    $e"
        }
        return $false
    }
    else {
        return $true
    }
}

### Execution

$syntaxErrors = 0
git diff-index --name-only --cached HEAD |% {
    if (($_ -match ".*\.($psExtensions)$") -and (test-path $_)) {
        if (-not (Test-PowershellSyntax -filename $_)) {
            $syntaxErrors += 1 
        }
    }
}

if ($syntaxErrors -gt 0) {
    write-host -foreground Red "Some files have syntax errors. Please fix then commit"
    exit 1
}
