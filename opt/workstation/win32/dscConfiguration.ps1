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

Configuration DecrapifyWindowsServerConfig {
    Import-DscResource -ModuleName PSDesiredStateConfiguration

    Node $ComputerName {

        Registry "DoNotOpenServerManagerAtLogon" {
            Ensure = "Present"
            Force = $true
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\ServerManager"
            ValueName = "DoNotOpenServerManagerAtLogon"
            Hex = $true
            ValueData = "0x1"
            ValueType = "Dword"
        }

    }
}