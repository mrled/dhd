<#
.synopsis
DSC configuration for configuring a new workstation the way I prefer
#>

<#
Enable debugging crap.
Not useful once everything is fully automated, but they're annoying the fuck out of me when I'm RDPing to the server all the time during debugging.
Note that this *should not* do anything that requires a restart, since we call it with `Start-DscConfiguration -Wait`
#>
Configuration DhdConfig {
    param(
        [string[]] $ComputerName = "localhost",
        [string] $UserProfile = "${env:USERPROFILE}",
        [string] $AppData = "${env:AppData}",
        [string] $VsCodePath = "${env:ProgramFiles}\Microsoft VS Code\bin\code.cmd",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlFileLink

    Node $ComputerName {

        File EnableWindowsPowershellProfile {
            DestinationPath = "$UserProfile\Documents\WindowsPowershell\profile.ps1"
            SourcePath = "$UserProfile\.dhd\hbase\Microsoft.PowerShell_profile.win32.ps1"
            Ensure = "Present"
            Type = "File"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            MatchSource = $true
        }
        File EnablePowershellCoreProfile {
            DestinationPath = "$UserProfile\Documents\Powershell\profile.ps1"
            SourcePath = "$UserProfile\.dhd\hbase\Microsoft.PowerShell_profile.win32.ps1"
            Ensure = "Present"
            Type = "File"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            MatchSource = $true
        }

        cMrlFileLink SymlinkKnownHosts {
            LinkPath = "$UserProfile\.ssh\known_hosts"
            LinkTarget = "$UserProfile\.dhd\hbase\known_hosts"
            Ensure = "Present"
        }

        cMrlFileLink SymlinkVsCodeConfig {
            LinkPath = "$AppData\Code\User"
            LinkTarget = "$UserProfile\.dhd\opt\vscodeuser"
            Ensure = "Present"
        }

        Script InstallVsCodePackages {
            GetScript = { return @{ Result = "" } }
            TestScript = { return $false }
            SetScript = {
                $extensionsTxtPath = "$using:AppData\Code\User\extensions.txt"
                if (Test-Path -LiteralPath $extensionsTxtPath) {
                    $extensions = Get-Content -LiteralPath $extensionsTxtPath
                    foreach ($extension in $extensions) {
                        Write-Verbose -Message "Installing VS Code extension '$extension'..."
                        & $using:VsCodePath --install-extension "$extension"
                    }
                }
            }
        }

        # This will save the list to the extensions.txt file, ready to be checked in to Git
        # Not useful for new boxes, only useful when we apply this DSC config to boxes I've been using
        # Does not commit this to Git, you have to do that yourself
        Script SaveVsCodePackages {
            GetScript = { return @{ Result = "" } }
            TestScript = { return $false }
            SetScript = {
                $extensionsTxtPath = "$using:AppData\Code\User\extensions.txt"
                & $using:VsCodePath --list-extensions | Out-File -LiteralPath $extensionsTxtPath -Encoding utf8
            }
        }

        cMrlFileLink SymlinkSublimeConfig {
            LinkPath = "$AppData\Sublime Text 3\Packages\User"
            LinkTarget = "$UserProfile\.dhd\opt\sublimetextuser"
            Ensure = "Present"
        }

        # On first startup, Package Control will then run and install all my packages, nice
        Script InstallSublimeTextPackageControl {
            GetScript = { return @{ Result = "" } }
            TestScript = {
                Test-Path -LiteralPath "$using:AppData\Sublime Text 3\Installed Packages\Package Control.sublime-package"
            }
            SetScript = {
                $pkgUri = "https://packagecontrol.io/Package%20Control.sublime-package"
                $pkgPath = "$using:AppData\Sublime Text 3\Installed Packages\Package Control.sublime-package"
                New-Item -Type Directory -Force -Path $(Split-Path -LiteralPath $pkgPath) | Out-Null
                Invoke-WebRequest -Uri $pkgUri -OutFile $pkgPath
            }
        }

    }
}

Configuration UserRegistrySettingsConfig {
    param(
        [string[]] $ComputerName = "localhost",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration

    Node $ComputerName {

        Registry "DisallowAppsFromUsingAdvertisingId" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo"
            ValueName = "Enabled"
            ValueData = 0
            ValueType = "Dword"
        }
        Registry "DisableBingSearchResultsInStartMenu" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Search"
            ValueName = "BingSearchEnabled"
            ValueData = 0
            ValueType = "Dword"
        }
        Registry "ShowHiddenFiles" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "Hidden"
            ValueData = 1
            ValueType = "Dword"
        }
        Registry "ShowFileExtensions" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "HideFileExt"
            ValueData = 0
            ValueType = "Dword"
        }
        Registry "SetExplorerHomeScreenToThisPc" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "LaunchTo"
            ValueData = 1  # Default value: 2
            ValueType = "Dword"
        }
        Registry "ExpandExplorerNavigationPaneToCurrentFolder" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "NavPaneExpandToCurrentFolder"
            ValueData = 1
            ValueType = "Dword"
        }
        Registry "ShowAllFoldersInExplorerNavigationPane" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "NavPaneShowAllFolders"
            ValueData = 1
            ValueType = "Dword"
        }
        Registry "ShowExplorerStatusBar" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "ShowStatusBar"
            ValueData = 1
            ValueType = "Dword"
        }
        Registry "DisableSharingWizard" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "SharingWizardOn"
            ValueData = 0
            ValueType = "Dword"
        }
        Registry "DisablePeopleInTaskbar" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\People"
            ValueName = "PeopleBand"
            ValueData = 0
            ValueType = "Dword"
        }
        Registry "NeverHideSystemTrayIcons" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer"
            ValueName = "EnableAutoTray"
            ValueData = 0
            ValueType = "Dword"
        }

    }
}

Configuration MachineSettingsConfig {
    param(
        [string[]] $ComputerName = "localhost"
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration

    Node $ComputerName {

        # Only applicable on Windows Server... TODO: restrict this to only Windows Server
        Registry "Do Not Open Server Manager At Login" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\ServerManager"
            ValueName = "DoNotOpenServerManagerAtLogon"
            ValueData = 1
            ValueType = "DWord"
        }

        # Set CapsLock to Ctrl
        # From original in pure Powershell:
        #   $layoutBytes = ([byte[]] @(0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x1d,0x00,0x3a,0x00,0x00,0x00,0x00,0x00))
        #   Set-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout" -Name "Scancode Map" -Type Binary -Value $layoutBytes
        # See also: https://serverfault.com/questions/865450/dsc-syntax-for-binary-registry-key
        Registry "Set CAPSLOCK to CTRL" {
            Key = "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout"
            ValueName = "Scancode Map"
            ValueData = "0000000000000000020000001d003a0000000000"
            ValueType = "Binary"
        }

        # Disabling telemetry requires a reboot to take effect
        Registry "Disable Telemetry: Registry Keys" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\DataCollection"
            ValueName = "AllowTelemetry"
            ValueData = 0
            ValueType = "DWord"
        }
        Service "Disable Telemetry: Connected User Experiences and Telemetry Service" {
            Name = "DiagTrack"
            StartupType = "Disabled"
            State = "Stopped"
        }
        Service "Disable Telemetry: WAP Push Message Routing Service" {
            Name = "Dmwappushservice"
            StartupType = "Disabled"
            State = "Stopped"
        }

        Registry "Disable Automatic Installation of Bullshit Apps that Microsoft Gets Paid To Push, Seriously?" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\CloudContent"
            ValueName = "DisableWindowsConsumerFeatures"
            ValueData = 1
            ValueType = "DWord"
        }

        Registry "Disable WiFi Sense HotSpot Sharing" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\PolicyManager\default\WiFi\AllowWiFiHotSpotReporting"
            ValueName = "Value"
            ValueData = 0
            ValueType = "DWord"
        }
        Registry "Disable WiFi Sense Shared HotSpot Auto-Connect" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\PolicyManager\default\WiFi\AllowAutoConnectToWiFiSenseHotspots"
            ValueName = "Value"
            ValueData = 0
            ValueType = "DWord"
        }

    }
}