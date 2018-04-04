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
                'Less'
                'SublimeText3'
                'VisualStudioCode'
                'bind-toolsonly'
                'curl'
                'docker-for-windows'
                'golang'
                'gpg4win-vanilla'
                'greenshot'
                'mRemoteNG'
                'metapad'
                'nodejs'
                'packer'
                'pandoc'
                'powershell-core'
                'pt'
                'putty'
                'python2'
                'python3'
                'slack'
                'sysinternals'
                'trid'
                'vagrant'
                'vim'
                'visualstudio2017buildtools'
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
        [string] $LocalAppData = "${env:LocalAppData}",
        [string] $VsCodePath = "${env:ProgramFiles}\Microsoft VS Code\bin\code.cmd",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlFileLink
    Import-DscResource -ModuleName cMrlUserEnvironment
    Import-DscResource -ModuleName cMrlPathLikeEnvVar
    Import-DscResource -ModuleName cMrlPathLikeEnvVarSet

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

        File EnableWindowsPowershellProfile {
            DestinationPath = "$UserProfile\Documents\WindowsPowershell\profile.ps1"
            Contents = ". $Home\.dhd\hbase\profile.ps1"
            Ensure = "Present"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            PsDscRunAsCredential = $Credential
        }
        File EnablePowershellCoreProfile {
            DestinationPath = "$UserProfile\Documents\Powershell\profile.ps1"
            Contents = ". $Home\.dhd\hbase\profile.ps1"
            Ensure = "Present"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            PsDscRunAsCredential = $Credential
        }

        cMrlFileLink SymlinkKnownHosts {
            LinkPath = "$UserProfile\.ssh\known_hosts"
            LinkTarget = "$DhdPath\hbase\known_hosts"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        Script ConfigureSshHashKnownHosts {
            GetScript = { return @{ Result = "" } }
            TestScript = {
                return $(Test-Path -Path $using:UserProfile\.ssh\config) -And
                    $(Select-String -Path $using:UserProfile\.ssh\config -Pattern 'HashKnownHosts Yes' -Quiet)
            }
            SetScript = {
                # Work around an issue with string that was triggered by a string like
                # "${using:UserProfile}\.ssh\config"
                $uprof = $using:UserProfile
                if (-not (Test-Path -Path $uprof\.ssh\config)) {
                    New-Item -Force -ItemType Directory -Path $uprof\.ssh
                    New-Item -ItemType File -Path $uprof\.ssh\config
                }
                $sshConfigContent = @(
                    "HashKnownHosts Yes"
                    Get-Content -LiteralPath $uprof\.ssh\config
                )
                [IO.File]::WriteAllLines(
                    "$uprof\.ssh\config",
                    $sshConfigContent,
                    $(New-Object -TypeName System.Text.UTF8Encoding -ArgumentList @($False) ))
            }
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

        cMrlPathLikeEnvVarSet "PrependPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "$UserProfile\.dhd\opt\bin"
                "$UserProfile\.dhd\opt\powershell\bin"
            )
            Ensure = "Present"
            InsertionMode = "Prepend"
            OnlyIfExists = $true
            PsDscRunAscredential = $Credential
        }

    }
}

Configuration UserSettingsConfig {
    param(
        [string[]] $ComputerName = "localhost",
        [string] $AppData = "${env:AppData}",
        [string] $LocalAppData = "${env:LocalAppData}",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlUserEnvironment
    Import-DscResource -ModuleName cMrlGitGlobalConfiguration
    Import-DscResource -ModuleName cMrlPathLikeEnvVar
    Import-DscResource -ModuleName cMrlPathLikeEnvVarSet

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

        # Less command-line arguments
        # -i: Ignore case in searches that do not contain uppercase.
        # -R: Output "raw" control characters.
        # -c: Repaint by clearing rather than scrolling.
        # -F: Quit if entire file fits on first screen. (Don't use with -c, lol)
        cMrlUserEnvironment "SetLessEnvVar" {
            Name = "LESS"
            Value = "-iRc"
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

        cMrlGitGlobalConfiguration "GitSetUsername" {
            Name = "user.name"
            Value = "Micah R Ledbetter"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        cMrlGitGlobalConfiguration "GitSetUserEmail" {
            Name = "user.email"
            Value = "me@micahrl.com"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # By default, 'git branch' commands are piped to a pager, which is annoying
        cMrlGitGlobalConfiguration "GitDisablePagerForBranchCommand" {
            Name = "pager.branch"
            Value = "false"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # Requires subl.exe to be in $env:PATH
        cMrlGitGlobalConfiguration "GitConfigureSublimeTextAsEditor" {
            Name = "core.editor"
            Value= "subl.exe -w"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # As of 20180221, the "Less" Chocolatey package is version 529
        # - No problems refreshing screen
        # - Displays colors from ANSI escape sequences properly
        # - It's much faster than relying on vim's less.vim package
        # Requires less.exe to be in $env:PATH
        cMrlGitGlobalConfiguration "GitConfigureLessPager" {
            Name = "core.pager"
            Value = "${env:ChocolateyInstall}\bin\less.exe" -Replace "\\","\\"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # If we have an ssh.exe in the path, assume it's Microsoft's OpenSSH port
        # - Lets us use ~/.ssh/config, ~/.ssh/id_* keys, etc
        # - Tested and works well
        # Requires ssh.exe to be in $env:PATH
        cMrlGitGlobalConfiguration "GitConfigureSsh" {
            Name = "core.sshCommand"
            Value = "ssh.exe"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # I don't remember if this gets set by default?
        cMrlUserEnvironment "SetGoPathEnvVar" {
            Name = "GOPATH"
            Value = "$env:USERPROFILE\Documents\Go"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        cMrlPathLikeEnvVarSet "PrependPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "$UserProfile\opt\bin"
                "$LocalAppData\Continuum\miniconda3"
                "$LocalAppData\Continuum\miniconda3\Scripts"
            )
            Ensure = "Present"
            InsertionMode = "Prepend"
            OnlyIfExists = $true
            PsDscRunAscredential = $Credential
        }

        cMrlPathLikeEnvVarSet "AppendPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "${AppData}\npm"
                "${AppData}\Python\Python3*\Scripts"
                "${AppData}\Python\Python2*\Scripts"
                "${LocalAppData}\atom\bin"
                "${LocalAppData}\Pandoc"
                "${LocalAppData}\Keybase"
                "${env:GOROOT}\bin"
                "${env:GOPATH}\bin"
            )
            Ensure = "Present"
            InsertionMode = "Append"
            OnlyIfExists = $true
            PsDscRunAscredential = $Credential
        }

    }
}

Configuration MachineSettingsConfig {
    param(
        [string[]] $ComputerName = "localhost"
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlPathLikeEnvVar
    Import-DscResource -ModuleName cMrlPathLikeEnvVarSet

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

        cMrlPathLikeEnvVarSet "PrependPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "${env:ChocolateyInstall}\bin"
                "${env:SystemDrive}\ProgramData\Chocolatey\bin"

                "${env:SystemDrive}\Perl64"
                "${env:SystemDrive}\Python3*"
                "${env:SystemDrive}\Python3*\Scripts"
                "${env:SystemDrive}\Python2*"
                "${env:SystemDrive}\Python2*\Scripts"
                "${env:SystemDrive}\Tools\Go\bin"
                "${env:SystemDrive}\Tools\mingw64\bin"
                "${env:SystemDrive}\Tools\Python3*"
                "${env:SystemDrive}\Tools\Python3*\Scripts"
                "${env:SystemDrive}\Tools\Ruby*\bin"

                "${env:ProgramFiles}\7-zip"
                "${env:ProgramFiles}\Amazon\AWSCLI"
                "${env:ProgramFiles}\ConEmu"
                "${env:ProgramFiles}\ConEmu\ConEmu"
                "${env:ProgramFiles}\Docker\Docker\Resources\bin"
                "${env:ProgramFiles}\Docker\Docker\Resources\qemu-img"
                "${env:ProgramFiles}\Docker\Docker\Resources"
                "${env:ProgramFiles}\Git\cmd"
                "${env:ProgramFiles}\GNU\GnuPG"
                "${env:ProgramFiles}\Graphviz*\bin"
                "${env:ProgramFiles}\Kdiff3"
                "${env:ProgramFiles}\LLVM\bin"
                "${env:ProgramFiles}\Microsoft Visual Studio*\VC\bin"
                "${env:ProgramFiles}\Microsoft VS Code\bin"
                "${env:ProgramFiles}\MSBuild\*\Bin"
                "${env:ProgramFiles}\NUnit*\bin"
                "${env:ProgramFiles}\OpenSSH"
                "${env:ProgramFiles}\OpenSSL\bin"
                "${env:ProgramFiles}\Oracle\VirtualBox"
                "${env:ProgramFiles}\Powershell\*"
                "${env:ProgramFiles}\Nmap"
                "${env:ProgramFiles}\PuTTY"
                "${env:ProgramFiles}\Sublime Text 3"
                "${env:ProgramFiles}\WordNet\2.1\bin"
                "${env:ProgramFiles}\ZeroTier\One"
                "${env:ProgramFiles}\vim\vim*"
                "${env:ProgramFiles}\vim\vim*\macros"

                "${env:ProgramFiles(x86)}\GNU\GnuPG"
                "${env:ProgramFiles(x86)}\vim\vim*"
                "${env:ProgramFiles(x86)}\vim\vim*\macros"
            )
            Ensure = "Present"
            InsertionMode = "Prepend"
            OnlyIfExists = $true
        }
    }
}
