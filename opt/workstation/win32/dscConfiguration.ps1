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
    Import-DscResource -ModuleName cMrlFileLink

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

        cMrlFileLink SymlinkKnownHosts {
            LinkPath = "$env:USERPROFILE\.ssh\known_hosts"
            LinkTarget = "$env:USERPROFILE\.dhd\hbase\known_hosts"
            Ensure = "Present"
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