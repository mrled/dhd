
# .description
# Internal function that makes sure path-resolver is loaded
# We don't want to load this function automatically, and we don't want to assume path-resolver is loaded when it starts, so we resort to this
function Get-PathResolverContext {
    [CmdletBinding()] Param()
    if (-not (Get-Module |? -Property Name -eq path-resolver)) {
        throw "path-resolver is not imported"
    } else {
        Write-Verbose "Found path-resolver module"
    }
}

# .description
# Internal function to invoke git and throw an error
function Invoke-Git {
    [CmdletBinding()]
    param(
        [switch] $WhatIf,
        [string] $repositoryPath = $pwd,
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args
    )
    $commandLine = "git $args"

    # Git is noisy, and we are working with multiple repos in this module, so we always show a header
    $repo = Split-Path -Leaf $repositoryPath
    $root = Split-Path -Parent $repositoryPath
    # Write-Host -ForegroundColor Magenta "`r`n${repo}: $commandLine"
    # Write-Host -ForegroundColor Magenta "`r`n${commandLine}    [$repo]"
    Write-Host -ForegroundColor Green -NoNewLine "`r`n${commandLine}"
    Write-Host -ForegroundColor Blue "    [$repo]"

    if (-not $WhatIf) {
        Push-Location $repositoryPath
        try {
            Invoke-Expression $commandLine
            if ($LASTEXITCODE) {
                Write-Error "Git error while trying to run '$commandLine'"
            }
        } finally {
            Pop-Location
        }
    }
}

# .description
# Invoke a Git command (like 'status' or 'diff') on all path-resolver repositories
function Invoke-PathResolverGitCommand {
    [CmdletBinding()] Param(
        [Parameter(Position=0, ValueFromRemainingArguments=$true)] $Args
    )
    Get-PathResolverContext
    foreach ($repository in (Get-RepositoryRoot)) {
        Invoke-Git -repositoryPath $repository @Args
    }
}
set-alias prgit Invoke-PathResolverGitCommand

# .description
# "initialize powershell for (edfi) development" aka dot-source the script
function Initialize-PowershellForDevelopment {
    [CmdletBinding()] Param(
        [switch] $force
    )
    if (Get-Module |? -Property Name -eq InitializeDevelopmentEnvironment) {
        if ($force) {
            Remove-Module InitializeDevelopmentEnvironment
        } else {
            return
        }
    }
    . R:\mains\Ed-Fi-Ods-Implementation\Initialize-PowershellForDevelopment.ps1
}

# .description
# The fucking text templates pollute $env:TEMP with bullshit. Jesus christ.
function Nuke-EdFiTTBullshit {
    [CmdletBinding()] Param()
    $ttRegex = '^[0-9a-f]{32}\.(dll|log|pdb|cs)$'
    $ttbsDir = mkdir -force $env:TEMP\ttBullshit
    Get-ChildItem -Path $env:TEMP | Where-Object -Property Name -Match $ttRegex | Move-Item -Destination $ttbsDir
}

set-alias edfiinit Initialize-PowershellForDevelopment
