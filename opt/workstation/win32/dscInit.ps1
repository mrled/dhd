#Requires -Version 5
#Requires -RunAsAdministrator

<#
.SYNOPSIS
Configure a new Windows workstation the way I like it
.DESCRIPTION
Initialize a new Windows workstation
.PARAMETER UserCredential
The credential for the local account you wish to use. This account should already be created on the local machine, but you need not be logged into it now.
Note that it works with local, domain, or Microsoft accounts used to log in to Windows.
For a local account, just enter the account name and password.
For a Microsoft account, enter the email address as the account name (like user@example.com) and the Microsoft account password.
For a domain account, enter EXAMPLE\username or username@example.com for the username and the domain account password.
.PARAMETER TestRemote
Normally, this script will try to determine if it's being run from inside a checked-out dhd repository or not.
If it is, then it uses relative paths to find items in dhd that it depends on.
If it is not, then it downloads the latest code from the dhd master branch to a temporary directory.
This switch bypasses that check, forcing it to download the latest from GitHub.
.PARAMETER SkipPreSteps
Skip pre steps, including enabling PSRemoting and installing prereqs.
Intended for use in development.
.PARAMETER CalledFromSelf
Internal use only. Used to prevent an infinite loop.
.PARAMETER ConfigurationName
Only apply configurations that match this filter. Configuration names are defined in *.ps1 files in the dscConfigurations directory.
.EXAMPLE
Invoke-WebRequest -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/workstation/win32/dscInit.ps1 | Invoke-Expression
Must be run from an administrative prompt
.EXAMPLE
Invoke-WebRequest -Headers @{"Cache-Control"="no-cache"} -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/workstation/win32/dscInit.ps1 | Invoke-Expression
When testing, run this way to prevent Invoke-WebRequest from caching the response
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory)] [PSCredential] $UserCredential,
    [switch] $TestRemote,
    [switch] $SkipPreSteps,
    [switch] $CalledFromSelf,
    $ConfigurationName = '*'
)

# Comments for the Requires settings at the top of this file
# (Comments cannot precede "Requires" statements, so this is down here)
# - Requires -Version 5:            We use Powershell 5.x concepts
# - Requires -RunAsAdministrator:   This script sets machine settings and must be run as an admin
# - Requires -PSEdition Desktop:    Not present, but true
#                                   This is only 5.1 while we want to work on newly imaged 5.0 machines too

$ErrorActionPreference = "Stop"


## Globals I'll use later

$DhdZipUri = "https://github.com/mrled/dhd/archive/master.zip"
$RequiredDscModules = @(
    'cChoco'
    'xComputerManagement'
    'xHyper-V'
)
$MinimumMaxEnvelopeSize = 8192


## Helper Functions

<#
.SYNOPSIS
Create a temporary directory
#>
function New-TemporaryDirectory {
    [CmdletBinding()] Param()
    do {
        $newTempDirPath = Join-Path $env:TEMP (New-Guid | Select-Object -ExpandProperty Guid)
    } while (Test-Path -Path $newTempDirPath)
    New-Item -ItemType Directory -Path $newTempDirPath
}

<#
.SYNOPSIS
Invoke a Powershell DSC configuration
.DESCRIPTION
Invoke a Powershell DSC configuration by compiling it to a temporary directory,
running it immediately from that location,
and then removing the temporary directory.
.PARAMETER Name
The name of the DSC configuration to invoke
.PARAMETER Parameters
Parameters to pass to the DSC configuration
#>
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
.SYNOPSIS
Remove a value from a PATH-like environment variable
.PARAMETER Name
The name of the environment variable
.PARAMETER Value
The value to remove from the environment variable
.PARAMETER TargetLocation
The environment location
#>
function Remove-PathLikeEnvironmentVariableValue {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name,
        [Parameter(Mandatory)] [string] $Value,
        [Parameter(Mandatory)] [ValidateSet('Process', 'User', 'Machine')] [string[]] $TargetLocation
    )
    foreach ($location in $TargetLocation) {
        $currentValue = [Environment]::GetEnvironmentVariable($Name, $location)
        $currentValueSplit = $currentValue -Split ';'
        if ($currentValueSplit -Contains $Value) {
            Write-Verbose -Message "Removing value '$Value' from '$location' '$Name' environment variable"
            $newValue = ($currentValueSplit | Foreach-Object -Process { if ($_ -ne $Value) {$_} }) -Join ";"
            [Environment]::SetEnvironmentVariable($Name, $newValue, $location)
        } else {
            Write-Verbose -Message "The value '$Value' is not a member of '$location' '$Name' environment variable"
        }
    }
}

<#
.SYNOPSIS
Append a value to a PATH-like environment variable
.PARAMETER Name
The name of the environment variable
.PARAMETER Value
The value to add to the environment variable
.PARAMETER  Location
The environment location
#>
function Add-PathLikeEnvironmentVariableValue {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name,
        [Parameter(Mandatory)] [string] $Value,
        [Parameter(Mandatory)] [ValidateSet('Process', 'User', 'Machine')] [string[]] $TargetLocation
    )
    foreach ($location in $TargetLocation) {
        $currentValue = [Environment]::GetEnvironmentVariable($Name, $location)
        $currentValueSplit = $currentValue -Split ';'
        if ($currentValueSplit -NotContains $Value) {
            Write-Verbose -Message "Adding value '$Value' to '$location' '$Name' environment variable"
            [Environment]::SetEnvironmentVariable($Name, "$currentValue;$Value", $location)
        } else {
            Write-Verbose -Message "The value '$Value' is already a member of the '$location' '$Name' environment variable"
        }
    }
}

<#
.SYNOPSIS
Test whether WinRM aka Powershell Remoting has been enabled
#>
function Test-WinRmEnabled {
    [CmdletBinding()] Param()
    try {
        Invoke-Command -ComputerName localhost -ScriptBlock { Write-Output "WinRM is enabled" }
        return $true
    } catch {
        return $false
    }
}

<#
.DESCRIPTION
Ensure that WinRM aka Powershell Remoting has been enabled and that its settings will work for us
#>
function Enable-PsRemotingForce {
    [CmdletBinding()] Param()
    $publicNetConnProfs = Get-NetConnectionProfile | Where-Object -Property NetworkCategory -EQ Public
    try {
        # Because ~*~FUCKING WINDOWS~*~ if you have ANY network connection profile set to "Public",
        # you cannot set MaxEnvelopeSizekb or enable PSRemoting. What.
        if ($publicNetConnProfs) {
            Set-NetConnectionProfile -InterfaceIndex $publicNetConnProfs.InterfaceIndex -NetworkCategory Private
        }
        if (-not (Test-WinRmEnabled)) {
            Enable-PSRemoting -SkipNetworkProfileCheck -Force
        }
        # Get the current max envelope size _after_ enabling remoting, or else the path doesn't exist
        $currentMaxEnvSize = Get-Item -LiteralPath WSMan:\localhost\MaxEnvelopeSizekb | Select-Object -ExpandProperty Value
        if ($currentMaxEnvSize -lt $script:MinimumMaxEnvelopeSize) {
            Set-Item -LiteralPath WSMan:\localhost\MaxEnvelopeSizekb -Value 2048
        }
    } finally {
        if ($publicNetConnProfs) {
            Set-NetConnectionProfile -InterfaceIndex $publicNetConnProfs.InterfaceIndex -NetworkCategory Public
        }
    }
}

<#
.SYNOPSIS
Get the dhd repository we are executing from
.PARAMETER Remote
Download the dhd zipfile and extract it to a temporary directory
.PARAMETER Local
Use the dhd repository already on disk where this file resides
Requires that this file be run from a checked-out dhd repository
.OUTPUTS
An object with these properties:
- DhdLocation:  The location of the dhd repository
- DhdTemp:      $false if -Local was passed, otherwise the path to the temporary directory where dhd is checked out
#>
function Get-DhdRepository {
    [CmdletBinding(DefaultParameterSetName='Remote')] Param(
        [Parameter(Mandatory, ParameterSetName='Remote')] [switch] $Remote,
        [Parameter(Mandatory, ParameterSetName='Local')] [switch] $Local
    )
    if ($Remote) {
        Write-Verbose -Message "Downloading dhd from '$script:DhdZipUri'..."
        $dhdTemp = New-TemporaryDirectory
        $dhdTempZip = "$dhdTemp\dhd-temp.zip"

        # I am not sure why, but by default, this request will sometimes fail if we don't enable TLS1.2 like this:
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        Invoke-WebRequest -UseBasicParsing -Uri $script:DhdZipUri -OutFile $dhdTempZip

        Expand-Archive -LiteralPath $dhdTempZip -DestinationPath $dhdTemp
        # We rely on the fact that GitHub zipfiles contain a single subdir with all repo items inside
        $dhdLocation = Get-ChildItem -Directory -LiteralPath $dhdTemp | Select-Object -ExpandProperty FullName
    } elseif ($Local) {
        Write-Verbose -Message "Using on-disk dhd..."
        $dhdLocation = Resolve-Path -LiteralPath $PSScriptRoot\..\..\..\ | Select-Object -ExpandProperty Path
        $dhdTemp = $false
    } else {
        throw "Unable to determine parameter set"
    }

    New-Object -TypeName PSObject -Property @{
        DhdLocation = $dhdLocation
        DhdTemp = $dhdTemp
    }
}

<#
.SYNOPSIS
Install modules required by out DSC configurations
#>
function Install-DscPrerequisites {
    [CmdletBinding()] Param()
    try {
        Import-Module -Name PowerShellGet
    } catch {
        # This stuff is weird
        # Looks like you might need a new Powershell instance after adding the NuGet package provider?
        # And possibly another new PS instance after installing the PowerShellGet module?
        Install-PackageProvider -Name NuGet -Force
        # You must provide -Force here,
        # because a version of the PowerShellGet module ships with Windows,
        # and we must install a newer version of it side-by-side here with -Force
        Install-Module -Name PowerShellGet -Force
    }
    if ('PSGallery' -NotIn (Get-PSRepository | Where-Object -Property InstallationPolicy -eq 'Trusted' | Select-Object -ExpandProperty Name)) {
        Set-PSRepository -Name PSGallery -InstallationPolicy Trusted
    }
    foreach ($module in $script:RequiredDscModules) {
        if (-not (Get-Module -ListAvailable -Name $module)) {
            Install-Module -Name $module
        }
    }
}

<#
.SYNOPSIS
Invoke all our DSC configurations
#>
function Invoke-AllDscConfigurations {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] $DhdLocation,
        [Parameter(Mandatory)] [PSCredential] $UserCredential,
        $ConfigurationName = "*"
    )

    try {
        $dscInitModulePath = Resolve-Path -LiteralPath $DhdLocation\opt\powershell\modules | Select-Object -ExpandProperty Path

        # TODO: lkadjflakdjflakdj comment me why both machine and proess
        Add-PathLikeEnvironmentVariableValue -Name PSModulePath -Value $dscInitModulePath -TargetLocation $("Machine","Process")

        $UserCredentialConfigData = @{
            AllNodes = @(
                @{
                    NodeName = "localhost"
                    PSDscAllowPlainTextPassword = $true
                    PSDscAllowDomainUser = $true
                }
            )
        }

        Get-ChildItem -Path $DhdLocation\opt\workstation\win32\dscConfigurations\* -Include *.ps1 |
            Foreach-Object -Process { . $_ }

        if ("InstallSoftware" -Like $ConfigurationName) {
            Invoke-DscConfiguration -Name InstallSoftware
        }
        if ("MachineSettingsConfig") {
            Invoke-DscConfiguration -Name MachineSettingsConfig
        }
        if ("DhdConfig" -Like $ConfigurationName) {
            Invoke-DscConfiguration -Name DhdConfig -Parameters @{
                Credential = $UserCredential
                ConfigurationData = $UserCredentialConfigData
            }
        }
        if ("UserSettingsConfig" -Like $ConfigurationName) {
            Invoke-DscConfiguration -Name UserSettingsConfig -Parameters @{
                Credential = $UserCredential
                ConfigurationData = $UserCredentialConfigData
            }
        }

    } finally {
        # We don't want our module path to stick around in the _machine_'s module path
        Remove-PathLikeEnvironmentVariableValue -Name PSModulePath -Value $dscInitModulePath -TargetLocation "Machine"
    }

}

# Allow dot-sourcing the script without running
# (Useful during debugging)
# If dot-sourced, the following block will not run:

if ($MyInvocation.InvocationName -ne '.') {

    if (-not $SkipPreSteps) {
        # Set the Process/User exec policies to prevent a possible error when setting the LocalMachine policy
        # Setting the LocalMachine policy will throw an error if a more specific scope takes precedence
        foreach ($scope in @('Process', 'CurrentUser', 'LocalMachine')) {
            Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope $scope -Force
        }

        Enable-PsRemotingForce
        if ($TestRemote -Or -Not $PSScriptRoot) {
            if ($CalledFromSelf) {
                throw "Looks like we might be in an infinite loop, ejecting..."
            }
            $dhdRepo = Get-DhdRepository -Remote
            try {
                & "$($dhdRepo.DhdLocation)\opt\workstation\win32\dscInit.ps1" -Verbose -CalledFromSelf -UserCredential (Get-Credential)
            } finally {
                Remove-Item -Recurse -Force -LiteralPath $dhdRepo.DhdTemp
            }
            # Since we execute dscInit.ps1 from the just-downloaded dhd repo -
            # that is, another copy of this same script -
            # stop executing this copy of the script when that one finishes
            return
        } else {
            $dhdRepo = Get-DhdRepository -Local
        }

        Install-DscPrerequisites

    } else {
        $dhdRepo = Get-DhdRepository -Local
    }

    Invoke-AllDscConfigurations -DhdLocation $dhdRepo.DhdLocation -UserCredential $UserCredential -ConfigurationName $ConfigurationName
}
