<#
.synopsis
DSC configuration for configuring a new workstation the way I prefer
#>

Configuration InstallSoftware {
    Param(
        [string[]] $ComputerName = "localhost",
        [string] $ChocoInstallDir = $(Join-Path -Path ${env:ProgramData} -ChildPath "Chocolatey")
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cChoco

    Node $ComputerName {

        cChocoInstaller "InstallChoco" {
            # It seems like this should add $ChocoInstallDir\bin to PATH, but it doesn't appear to do so at the Machine level
            InstallDir = $ChocoInstallDir
        }
        Script "AddChocoBinToMachinePath" {
            GetScript = { return @{ Result = "" } }
            TestScript = {
                [Environment]::GetEnvironmentVariable("Path", "Machine") -split ';' -contains "$Using:ChocoInstallDir\bin"
            }
            SetScript = {
                $path = [Environment]::GetEnvironmentVariable("Path", "Machine") + [System.IO.Path]::PathSeparator + "$Using:ChocoInstallDir\bin"
                [Environment]::SetEnvironmentVariable("Path", $path, "Machine")
            }
        }

        # This module is in development but not yet released
        # cChocoFeature "Configure Chocolatey to never require confirmation" {
        #     FeatureName = "allowGlobalConfirmation"
        #     Ensure = "Present"
        # }
        Script "Configure Chocolatey to never require confirmation" {
            GetScript = { return @{ Result = "" } }
            TestScript = { return $false }
            SetScript = {
                choco feature enable --name=allowGlobalConfirmation --yes
            }
            DependsOn = "[Script]AddChocoBinToMachinePath"
        }

        # NOTE: Client OSes cannot use WindowsFeature DSC resources, so we are resigned to this
        cChocoPackageInstaller "InstallHyperV" {
            Name = "Microsoft-Hyper-V-All"
            Source = "windowsFeatures"
            DependsOn = '[cChocoInstaller]InstallChoco'
        }
        cChocoPackageInstaller "InstallWindowsSubsystemForLinux" {
            Name = "Microsoft-Windows-Subsystem-Linux"
            Source = "windowsFeatures"
            DependsOn = '[cChocoInstaller]InstallChoco'
        }

        # Packages with no parameters, alphabetical order
        cChocoPackageInstallerSet "ChocoInstallPackages" {
            Name = @(
                '7zip'
                'ConEmu'
                'Firefox'
                'GoogleChrome'
                'SublimeText3'
                'VisualStudioCode'
                'bind-toolsonly'
                'curl'
                'docker-for-windows'
                'golang'
                'gpg4win-vanilla'
                'greenshot'
                'less'
                'mRemoteNG'
                'metapad'
                'packer'
                'powershell-core'
                'pt'
                'putty'
                'python2'
                'python3'
                'slack'
                'sysinternals'
                'vagrant'
                'vim'
            )
            DependsOn = '[cChocoInstaller]InstallChoco'
        }

        # Packages with parameters, alphabetical order
        cChocoPackageInstaller "ChocoInstallGit" {
            Name = 'git'
            Params = "/GitOnlyOnPath"
            DependsOn = '[cChocoInstaller]InstallChoco'
        }
        cChocoPackageInstaller "ChocoInstallOpenSSH" {
            Name = 'openssh'
            Params = "/SSHServerFeature /SSHAgentFeature"
            DependsOn = '[cChocoInstaller]InstallChoco'
        }

    }
}

Configuration DhdConfig {
    param(
        [string[]] $ComputerName = "localhost",
        [string] $UserProfile = "${env:USERPROFILE}",
        [string] $DhdPath = "$UserProfile\.dhd",
        [string] $AppData = "${env:AppData}",
        [string] $VsCodePath = "${env:ProgramFiles}\Microsoft VS Code\bin\code.cmd",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlFileLink
    Import-DscResource -ModuleName cMrlUserEnvironment

    Node $ComputerName {

        Script CheckOutDhd {
            GetScript = { return @{ Result = "" } }
            TestScript = {
                Test-Path -LiteralPath "$Using:DhdPath\.git"
            }
            SetScript = {
                git clone "https://github.com/mrled/dhd" "$Using:DhdPath"
            }
            PsDscRunAsCredential = $Credential
        }

        # TODO: Fix error when this is uncommented:
        #   PowerShell DSC resource MSFT_ScriptResource  failed to execute Set-TargetResource functionality with error message: Exception setting "BackgroundColor": "A command that prompts the user failed because the host program or the command type does not support user interaction. Try a host program that supports user interaction, such as the Windows PowerShell Console or Windows PowerShell ISE, and remove prompt-related commands from command types that do not support user interaction, such as Windows PowerShell workflows."
        # Script InitialSetupPathAndEnv {
        #     GetScript = { return @{ Result = "" } }
        #     TestScript = { return $false }
        #     SetScript = {
        #         . "$Using:DhdPath\hbase\profile.ps1"
        #         Setup-SystemPath
        #         Setup-Environment
        #     }
        #     PsDscRunAsCredential = $Credential
        # }

        File EnableWindowsPowershellProfile {
            DestinationPath = "$UserProfile\Documents\WindowsPowershell\profile.ps1"
            SourcePath = "$DhdPath\hbase\profile.win32.ps1"
            Ensure = "Present"
            Type = "File"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            MatchSource = $true
            PsDscRunAsCredential = $Credential
        }
        File EnablePowershellCoreProfile {
            DestinationPath = "$UserProfile\Documents\Powershell\profile.ps1"
            SourcePath = "$DhdPath\hbase\profile.win32.ps1"
            Ensure = "Present"
            Type = "File"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            MatchSource = $true
            PsDscRunAsCredential = $Credential
        }

        cMrlFileLink SymlinkKnownHosts {
            LinkPath = "$UserProfile\.ssh\known_hosts"
            LinkTarget = "$DhdPath\hbase\known_hosts"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        cMrlFileLink SymlinkVsCodeConfig {
            LinkPath = "$AppData\Code\User"
            LinkTarget = "$DhdPath\opt\vscodeuser"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
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
            PsDscRunAsCredential = $Credential
        }

        cMrlFileLink "Symlink ConEmu configuration" {
            LinkPath = "$AppData\ConEmu.xml"
            LinkTarget = "$DhdPath\opt\win32\ConEmu.xml"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # This will save the list to the extensions.txt file, ready to be checked in to Git
        # Not useful for new boxes, only useful when we apply this DSC config to boxes I've been using
        # Does not commit this to Git, you have to do that yourself
        Script SaveVsCodePackages {
            GetScript = { return @{ Result = "" } }
            TestScript = { return $false }
            SetScript = {
                $extensionsTxtPath = "$using:DhdPath\opt\vscodeuser\extensions.txt"
                $extensions = & $using:VsCodePath --list-extensions

                # The following approach has two advantage over the much simpler Out-File:
                # 1. It uses only LF, not CRLF, for newlines
                # 2. It does not emit a UTF Byte Order Mark (BOM) at the beginning of the file
                # Both of these advantages are particularly helpful when working across operating systems

                if (Test-Path -LiteralPath $extensionsTxtPath) {
                    Remove-Item -LiteralPath $extensionsTxtPath
                }
                $outStream = New-Object -TypeName System.IO.StreamWriter -ArgumentList @($extensionsTxtPath, [Text.Encoding]::UTF8)
                $outStream.NewLine = "`n"
                foreach ($line in $extensions) {
                    $outStream.WriteLine($line)
                }
                $outStream.Close()
            }
            PsDscRunAsCredential = $Credential
        }

        cMrlFileLink SymlinkSublimeConfig {
            LinkPath = "$AppData\Sublime Text 3\Packages\User"
            LinkTarget = "$DhdPath\opt\sublimetextuser"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
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
            PsDscRunAsCredential = $Credential
        }

        cMrlUserEnvironment "SetPythonStartupEnvVar" {
            Name = "PYTHONSTARTUP"
            Value = "$DhdPath\hbase\python.profile"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }
        cMrlUserEnvironment "SetPythonPathEnvVar" {
            Name = "PythonPath"
            Value = "$DhdPath\opt\pythopn"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }

    }
}

Configuration UserSettingsConfig {
    param(
        [string[]] $ComputerName = "localhost",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlUserEnvironment

    Node $ComputerName {

        Registry "DisallowAppsFromUsingAdvertisingId" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo"
            ValueName = "Enabled"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "DisableBingSearchResultsInStartMenu" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Search"
            ValueName = "BingSearchEnabled"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowHiddenFiles" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "Hidden"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowFileExtensions" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "HideFileExt"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "SetExplorerHomeScreenToThisPc" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "LaunchTo"
            ValueData = 1  # Default value: 2
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ExpandExplorerNavigationPaneToCurrentFolder" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "NavPaneExpandToCurrentFolder"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowAllFoldersInExplorerNavigationPane" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "NavPaneShowAllFolders"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowExplorerStatusBar" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "ShowStatusBar"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "DisableSharingWizard" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "SharingWizardOn"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "DisablePeopleInTaskbar" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\People"
            ValueName = "PeopleBand"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "NeverHideSystemTrayIcons" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer"
            ValueName = "EnableAutoTray"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }

        # Important for things like less.exe, sometimes
        cMrlUserEnvironment "SetTermEnvVar" {
            Name = "TERM"
            Value = "xterm"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }

        # I think this is less necessary recently, but some unix software does still use this
        cMrlUserEnvironment "SetHomeEnvVar" {
            Name = "HOME"
            Value = "$env:USERPROFILE"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
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