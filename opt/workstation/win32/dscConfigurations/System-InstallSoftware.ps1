<#
.DESCRIPTION
DSC configuration to install software. Requires administrative privileges.
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
                'gnuwin32-coreutils.portable'
                'gnuwin32-make.portable'
                'gnuwin32-sed.install'
                'gnuwin32-grep.install'
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
