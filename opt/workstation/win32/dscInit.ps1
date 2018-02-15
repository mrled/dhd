#Requires -Version 5
#Requires -PSEdition Desktop
#Requires -RunAsAdministrator

<#
.synopsis
Configure a new Windows workstation the way I like it
.description
Initialize a new Windows workstation
.parameter UserCredential
The credential for the user to configure
.parameter TestRemote
Normally, this script will try to determine if it's being run from inside a checked-out dhd repository or not.
If it is, then it uses relative paths to find items in dhd that it depends on.
If it is not, then it downloads the latest code from the dhd master branch to a temporary directory.
This switch bypasses that check, forcing it to download the latest from GitHub.
.parameter CalledFromSelf
Internal use only. Used to prevent an infinite loop.
.example
Invoke-WebRequest -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/workstation/win32/dscInit.ps1 | Invoke-Expression
Must be run from an administrative prompt
.example
Invoke-WebRequest -Headers @{"Cache-Control"="no-cache"} -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/workstation/win32/dscInit.ps1 | Invoke-Expression
When testing, run this way to prevent Invoke-WebRequest from caching the response
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory)] [PSCredential] $UserCredential,
    [switch] $TestRemote,
    [switch] $CalledFromSelf
)

$ErrorActionPreference = "Stop"


## Globals I'll use later

$DhdZipUri = "https://github.com/mrled/dhd/archive/master.zip"
$RequiredDscModules = @('xHyper-V', 'cChoco')
$MinimumMaxEnvelopeSize = 2048


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

<#
.synopsis
Add or clear a location from the local machine's PSModulePath environment variable
.parameter Location
A location to ensure exists in the path
.parameter Clear
If passed, rather than ensuring the location exists in the path, ensure it is cleared from it
.notes
I hate how DSC deals with $env:PsModulePath
It needs all DSC modules to reside in the _machine_ PsModulePath environment variable
Whatever
#>
function Set-LocationInMachinePsModulePath {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Location,
        [switch] $Clear
    )
    $currentValue = [Environment]::GetEnvironmentVariable("PSModulePath", "Machine")
    $currentValueSplit = $currentValue -Split ';'
    $Location = Resolve-Path -LiteralPath $Location | Select-Object -ExpandProperty Path

    if ($currentValueSplit -NotContains $Location -And -Not $Clear) {
        Write-Verbose -Message "Setting Machine PSModulePath environment variable to include '$Location'"
        [Environment]::SetEnvironmentVariable("PSModulePath", "$currentValue;$Location", "Machine")
    } elseif ($currentValueSplit -Contains $Location -And $Clear) {
        Write-Verbose -Message "Removing '$Location' from Machine PSModulePath environment variable"
        $newValue = ($currentValueSplit | Foreach-Object -Process { if ($_ -ne $Location) {$_} }) -Join ";"
        [Environment]::SetEnvironmentVariable("PSModulePath", $newValue, "Machine")
    } else {
        Write-Verbose -Message "Nothign to change"
    }
}

function Test-WinRmEnabled {
    [CmdletBinding()] Param()
    try {
        Invoke-Command -ComputerName localhost -ScriptBlock { Write-Output "WinRM is enabled" }
        return $true
    } catch {
        return $false
    }
}


## Apply necessary Windows changes

# Enable Powershell Remoting / WinRM
if (-not (Test-WinRmEnabled)) {
    Enable-PSRemoting -SkipNetworkCheck -Force
}

# Make sure that the configured MaxEnvelopeSize is big enough. By default, it isn't. Of course.
$currentMaxEnvSize = Get-Item -LiteralPath WSMan:\localhost\MaxEnvelopeSizekb | Select-Object -ExpandProperty Value
if ($currentMaxEnvSize -lt $MinimumMaxEnvelopeSize) {
    # Because ~*~FUCKING WINDOWS~*~ if you have ANY network connection profile set to "Public", you cannot set MaxEnvelopeSizekb. What.
    $publicNetConnProfs = Get-NetConnectionProfile -NetworkCategory Public
    try {
        Set-NetConnectionProfile -InterfaceIndex $publicNetConnProfs.InterfaceIndex -NetworkCategory Private
        Set-Item -LiteralPath WSMan:\localhost\MaxEnvelopeSizekb -Value 2048
    } finally {
        # After setting MaxEnvelopeSizekb, you can set them back to Public and everything will still work fine
        Set-NetConnectionProfile -InterfaceIndex $publicNetConnProfs.InterfaceIndex -NetworkCategory Public
    }

}


## Get dhd

if ($TestRemote -Or -Not $PSScriptRoot) {
    if ($CalledFromSelf) {
        throw "Looks like we might be in an infinite loop, ejecting..."
    }
    Write-Verbose -Message "Downloading dhd from '$DhdZipUri'..."
    # If we aren't running this script from a local filesystem, assume we need to download and install dhd
    $dhdTemp = New-TemporaryDirectory
    $dhdTempZip = "$dhdTemp\dhd-temp.zip"
    Invoke-WebRequest -Uri $DhdZipUri -OutFile $dhdTempZip
    Expand-Archive -LiteralPath $dhdTempZip -DestinationPath $dhdTemp
    # We rely on the fact that GitHub zipfiles contain a single subdir with all repo items inside
    $dhdLocation = Get-ChildItem -Directory -LiteralPath $dhdTemp | Select-Object -ExpandProperty FullName
    & "$dhdLocation\opt\workstation\win32\dscInit.ps1" -Verbose -CalledFromSelf -UserCredential (Get-Credential)
    return
} else {
    Write-Verbose -Message "Using on-disk dhd..."
    $dhdLocation = Resolve-Path -LiteralPath $PSScriptRoot\..\..\..\ | Select-Object -ExpandProperty Path
    $dhdTemp = $false
}
Write-Verbose "Using dhd at location '$dhdLocation'"


## Install DSC prerequisites

Set-ExecutionPolicy RemoteSigned -Scope LocalMachine -Force

if ('NuGet' -notin (Get-PackageProvider | Select-Object -ExpandProperty Name)) {
    Install-PackageProvider -Name NuGet -Force
}
if ('PSGallery' -notin (Get-PSRepository | Where-Object -Property InstallationPolicy -eq 'Trusted')) {
    Set-PSRepository -Name PSGallery -InstallationPolicy Trusted
}
foreach ($module in $RequiredDscModules) {
    if (-not (Get-Module -ListAvailable -Name $module)) {
        Install-Module -Name $module
    }
}


## Apply DSC Configuration

try {
    Set-LocationInMachinePsModulePath -Location $dhdLocation\opt\powershell\modules

    $LocalhostConfigData = @{
        AllNodes = @(
            @{
                NodeName = "localhost"
                PSDscAllowPlainTextPassword = $true
                PSDscAllowDomainUser = $true
            }
        )
    }

    . $dhdLocation\opt\workstation\win32\dscConfiguration.ps1

    Invoke-DscConfiguration -Name InstallSoftware
    Invoke-DscConfiguration -Name MachineSettingsConfig
    Invoke-DscConfiguration -Name DhdConfig -Parameters @{
        Credential = $UserCredential
        ConfigurationData = $LocalhostConfigData
    }
    Invoke-DscConfiguration -Name UserRegistrySettingsConfig -Parameters @{
        Credential = $UserCredential
        ConfigurationData = $LocalhostConfigData
    }
} finally {
    # We don't want our module path to stick around in the _machine_'s module path
    Set-LocationInMachinePsModulePath -Location $dhdLocation\opt\powershell\modules -Clear

    # If we put dhd in a temp location, don't leave copies of it around
    if ($dhdTemp) {
        Remove-Item -Recurse -Force -LiteralPath $dhdTemp
    }
}
