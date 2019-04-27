<#
.DESCRIPTION
Download and apply DHD configuration
#>

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

                # Write a UTF8 file without a BOM
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
                        try {
                            & $using:VsCodePath --install-extension "$extension"
                        } catch {
                            Write-Verbose -Message "Failed to install VS code extension '$extension'"
                        }
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

        cMrlFileLink SymlinkVimrc {
            LinkPath = "$UserProfile\_vimrc"
            LinkTarget = "$DhdPath\hbase\.vimrc"
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
            Value = "$DhdPath\opt\python"
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
