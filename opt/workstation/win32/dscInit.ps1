<#
.synopsis
Configure a new Windows workstation the way I like it
.description
Initialize a new Windows workstation
.parameter UserCredential
The credential for the user to configure
.example
Invoke-WebRequest -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/workstation/win32/dscInit.ps1 | Invoke-Expression
Must be run from an administrative prompt
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory)] [PSCredential] $UserCredential
)

<#
Development notes:
- Cannot use $PSScriptRoot, because this script is executed directly from the weeb
- Cannot include DSC configuration blocks because of the Import-DscResource dynamic keyword
#>

$ErrorActionPreference = "Stop"

## Helper Functions

function New-TemporaryDirectory {
    [CmdletBinding()] Param()
    do {
        $newTempDirPath = Join-Path $env:TEMP (New-Guid | Select-Object -ExpandProperty Guid)
    } while (Test-Path -Path $newTempDirPath)
    New-Item -ItemType Directory -Path $newTempDirPath
}

## Apply DSC Configuration

$LocalhostConfigData = @{
    AllNodes = @(
        @{
            NodeName = "localhost"
            PSDscAllowPlainTextPassword = $true
            PSDscAllowDomainUser = $true
        }
    )
}

$dscWorkDir = New-TemporaryDirectory

. $PSScriptRoot\dscConfiguration.ps1

$dhdConfigParams = @{
    OutputPath = $dscWorkDir
    Credential = $UserCredential
    ConfigurationData = $LocalhostConfigData
}

try {
    DhdConfig @dhdConfigParams
    Start-DscConfiguration -Path $dscWorkDir
} finally {
    # Clean up the working directory because it contains an unencrypted credential
    Remove-Item -Recurse -Force -Path $dscWorkDir
}
