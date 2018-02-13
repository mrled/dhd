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
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration

    Node $ComputerName {

        File EnableWindowsPowershellProfile {
            DestinationPath = "$env:USERPROFILE\Documents\WindowsPowershell\profile.ps1"
            SourcePath = "$env:USERPROFILE\.dhd\hbase\Microsoft.PowerShell_profile.win32.ps1"
            Ensure = "Present"
            Type = "File"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            MatchSource = $true
        }
        File EnablePowershellCoreProfile {
            DestinationPath = "$env:USERPROFILE\Documents\Powershell\profile.ps1"
            SourcePath = "$env:USERPROFILE\.dhd\hbase\Microsoft.PowerShell_profile.win32.ps1"
            Ensure = "Present"
            Type = "File"
            Credential = $Credential
            Checksum = "SHA-512"
            Force = $true
            MatchSource = $true
        }

        Script "SymlinkKnownHosts" {
            GetScript = { return @{ Result = "" } }
            TestScript = {
                if (-not (Test-Path -Path $env:USERPROFILE\.ssh)) {
                    return $false
                }
                $item = Get-Item -Path $env:USERPROFILE\.ssh
                if ((Get-Member -InputObject $item | Select-Object -ExpandProperty Name) -Contains 'LinkType') {
                    if ($item.LinkType -eq 'SymbolicLink') {
                        return $true
                    }
                }
                return $false
            }
            SetScript = {
                New-Item -Type Directory -Force -Path $env:USERPROFILE\.ssh | Out-Null
                if (Test-Path -Path $env:USERPROFILE\.ssh\known_hosts) {
                    Remove-Item -Force -Path $env:USERPROFILE\.ssh\known_hosts
                }
                cmd /c mklink $env:USERPROFILE\.ssh\known_hosts $env:USERPROFILE\.dhd\hbase\known_hosts
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