<#
.synopsis
Set up Windows workstations with Boxstarter

.example
PS>
PS> Write-Host -Message "Installing Boxstarter via Chocolatey"
PS>
PS> Set-ExecutionPolicy Bypass -Scope Process -Force
PS> iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
PS> choco install boxstarter -y
PS> Get-Module -ListAvailable -Name "Boxstarter*" | Import-Module

Uses Chocolatey to install Boxstarter.

.example
PS>
PS> Write-Host -Message "Installing Boxstarter via the official, insecure way, because Boxstarter is amateur hour"
PS>
PS> Set-ExecutionPolicy Bypass -Scope Process -Force
PS> . { iwr -useb http://boxstarter.org/bootstrapper.ps1 } | iex
PS> Get-Boxstarter -Force

Installs Boxstarter the official way, over unencrypted HTTP. YOLO.

.example
PS>
PS> Write-Host -Message "Applying the Boxstarter configuration, once Boxstarter itself is installed"
PS>
PS> Install-BoxstarterPackage -DisableReboots -PackageName https://raw.githubusercontent.com/mrled/dhd/master/opt/powershell/boxstarter.ps1

Once Boxstarter has been installed and the module(s) imported, run this command to apply this Boxstarter configuration

.notes

When exploring new systems, these might be helpful:
    # List all installed programs
    Get-ItemProperty HKLM:\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall* | sort -property DisplayName | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate |Format-Table -AutoSize
    # List all store-installed programs
    Get-AppxPackage | sort -property Name | Select-Object Name, PackageFullName, Version | Format-Table -AutoSize

See also:
- https://gist.github.com/jessfraz/7c319b046daa101a4aaef937a20ff41f
- https://gist.github.com/NickCraver/7ebf9efbfd0c3eab72e9

To do items:
- Keybase
- Associate metapad with text files
- Start menu customization

#>
[CmdletBinding()] Param()

$ErrorActionPreference = "Stop"


# Utility functions

<#
.synopsis
Set a registry value, creating any requisite parent key(s)
#>
function Set-RegistryItemProperty {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Path,
        [Parameter(Mandatory)] [string] $Name,
        [Parameter(Mandatory)] [string] $Type,
        [Parameter(Mandatory)] $Value
    )
    if (-not (Test-Path  -Path $Path)) {
        New-Item -Path $Path | Out-Null
    }
    Set-ItemProperty -Path $Path -Name $Name -Type $Type -Value $Value
}


# (Undo this when finished)
Disable-UAC

# Windows settings
Disable-BingSearch
Disable-GameBarTips
Set-ExecutionPolicy Unrestricted -Scope LocalMachine -Force

# Privacy: Let apps use my advertising ID: Disable
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo -Name Enabled -Type DWord -Value 0

# WiFi Sense: HotSpot Sharing: Disable
Set-RegistryItemProperty -Path HKLM:\Software\Microsoft\PolicyManager\default\WiFi\AllowWiFiHotSpotReporting -Name value -Type DWord -Value 0

# WiFi Sense: Shared HotSpot Auto-Connect: Disable
Set-RegistryItemProperty -Path HKLM:\Software\Microsoft\PolicyManager\default\WiFi\AllowAutoConnectToWiFiSenseHotspots -Name value -Type DWord -Value 0

# Start Menu: Disable Bing Search Results
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Search -Name BingSearchEnabled -Type DWord -Value 0

# Disable Telemetry (requires a reboot to take effect)
# Note this may break Insider builds for your organization
Set-RegistryItemProperty -Path HKLM:\SOFTWARE\Policies\Microsoft\Windows\DataCollection -Name AllowTelemetry -Type DWord -Value 0
Get-Service DiagTrack,Dmwappushservice | Stop-Service | Set-Service -StartupType Disabled

# Show hidden files
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name Hidden -Type DWord -Value 1

# Show file extensions
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name HideFileExt -Type DWord -Value 0

# Change Explorer home screen back to "This PC" (default value: 2)
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name LaunchTo -Type DWord -Value 1

# Expand Explorer's navigation pane to the current folder
Set-RegistryItemProperty -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name NavPaneExpandToCurrentFolder -Type DWord -Value 1

# Show all folders in Explorer's navigation pane
Set-RegistryItemProperty -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name NavPaneShowAllFolders -Type DWord -Value 1

# Show Explorer status bar
Set-RegistryItemProperty -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name ShowStatusBar -Type DWord -Value 1

# Disable sharing wizard
Set-RegistryItemProperty -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name SharingWizardOn -Type DWord -Value 0

# Turn off People in Taskbar
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\People -Name PeopleBand -Type DWord -Value 0

# Do not hide system tray icons
Set-RegistryItemProperty -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer -Name EnableAutoTray -Type DWord -Value 0

# Set CapsLock to Ctrl
$layoutBytes = ([byte[]] @(0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x1d,0x00,0x3a,0x00,0x00,0x00,0x00,0x00))
Set-RegistryItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout" -Name "Scancode Map" -Type Binary -Value $layoutBytes


# Uninstall default crap
# Get-AppxPackage *Autodesk* | Remove-AppxPackage
# Get-AppxPackage *BubbleWitch* | Remove-AppxPackage
# Get-AppxPackage king.com.CandyCrush* | Remove-AppxPackage
# Get-AppxPackage *Facebook* | Remove-AppxPackage
# Get-AppxPackage *Keeper* | Remove-AppxPackage
# Get-AppxPackage *MarchofEmpires* | Remove-AppxPackage
# Get-AppxPackage *Minecraft* | Remove-AppxPackage
# Get-AppxPackage *Netflix* | Remove-AppxPackage
# Get-AppxPackage *Plex* | Remove-AppxPackage
# Get-AppxPackage *Twitter* | Remove-AppxPackage

# Chocolatey configuration
choco feature enable --name=allowGlobalConfirmation --yes

# Windows features
choco install Microsoft-Hyper-V-All --source=windowsFeatures
choco install Microsoft-Windows-Subsystem-Linux --source=windowsfeatures

# Applications
choco install ConEmu
choco install Firefox
choco install Gpg4win
choco install SublimeText3
choco install VisualStudioCode
choco install 7zip
choco install chrome
choco install docker-for-windows
choco install git --params '"/GitOnlyOnPath"'
choco install greenshot
choco install less
choco install metapad
choco install openssh --params '"/SSHServerFeature /SSHAgentFeature "'
choco install powershell-core
choco install putty
choco install python2
choco install python3
choco install slack
choco install sysinternals
choco install vim
refreshenv

# User settings
git clone https://github.com/mrled/dhd $env:USERPROFILE\.dhd
mkdir -Path $("$env:USERPROFILE\Documents\WindowsPowerShell", "$env:USERPROFILE\Documents\Powershell")
Copy-Item -Path $env:USERPROFILE\.dhd\hbase\Microsoft.Powershell_profile.win32.ps1 -Destination "$env:USERPROFILE\Documents\WindowsPowerShell\profile.ps1"
Copy-Item -Path $env:USERPROFILE\.dhd\hbase\Microsoft.Powershell_profile.win32.ps1 -Destination "$env:USERPROFILE\Documents\Powershell\profile.ps1"
Copy-Item -Path $env:USERPROFILE\.dhd\opt\win32\ConEmu.xml -Destination $env:AppData\ConEmu.xml
. $env:USERPROFILE\Documents\Powershell\profile.ps1
Setup-SystemPath
Setup-Environment

# Finishing up...
Enable-UAC
Enable-MicrosoftUpdate
Install-WindowsUpdate -acceptEula
