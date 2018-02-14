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
.notes
1. WinRM has to be enabled, via `Enable-PSRemoting -SkipNetworkCheck -Force`
2. WSMan has to have a large-ish MaxEnvelopeSize setting, via e.g.
  `Set-Item WSMan:\localhost\MaxEnvelopeSizekb 2048`
3. Because ~*~FUCKING WINDOWS~*~ if you have ANY network connection profile set to "Public",
   the above command will fail. See:
   `Get-NetConnectionProfile`
   `Set-NetConnectionProfile -NetworkCategory Private -Name YourConnectionName`
   Feel free to set them back to "Public" when you're done
4. It's possible to run DSC configs without admin access if you're providing a WinRM credential,
   as we do here. However, `Get-DscConfigurationStatus` won't work;
   you'll have to use `Receive-Job` instead
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

function Invoke-DscConfiguration {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name,
        [hashtable] $Parameters = @{}
    )

    $dscWorkDir = New-TemporaryDirectory
    $Parameters.OutputPath = $dscWorkDir.FullName
    Write-Verbose -Message "Using working directory at $($dscWorkDir.FullName)"

    try {
        & "$Name" @Parameters
        Start-DscConfiguration -Path $dscWorkDir -Wait -Force
    } finally {
        # Clean up the working directory because it may contain an unencrypted credential
        Remove-Item -Recurse -Force -Path $dscWorkDir
    }
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

# I hate how DSC deals with $env:PsModulePath
# It needs all DSC modules to reside in the _machine_ PsModulePath environment variable
# Whatever
# This will need to be adjusted later to pull the modules down so it works like magic from the web
$dhdPsModulePath = "$env:USERPROFILE\.dhd\opt\powershell\modules"
$machinePsModulePath = [Environment]::GetEnvironmentVariable("PSModulePath", "Machine")
if ($machinePsModulePath -NotContains $dhdPsModulePath) {
    [Environment]::SetEnvironmentVariable("PSModulePath", "$machinePsModulePath;$dhdPsModulePath", "Machine")
}

. $PSScriptRoot\dscConfiguration.ps1

Invoke-DscConfiguration -Name DhdConfig -Parameters @{
    Credential = $UserCredential
    ConfigurationData = $LocalhostConfigData
}
Invoke-DscConfiguration -Name UserRegistrySettingsConfig -Parameters @{
    Credential = $UserCredential
    ConfigurationData = $LocalhostConfigData
}
Invoke-DscConfiguration -Name MachineSettingsConfig -Parameters @{}
