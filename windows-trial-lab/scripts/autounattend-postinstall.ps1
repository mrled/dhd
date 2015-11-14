[cmdletbinding(DefaultParameterSetName="RunWindowsUpdates")] param(
    [Parameter(ParameterSetName="RunWindowsUpdates")] [switch] $RunWindowsUpdates,
    [Parameter(Mandatory=$true,ParameterSetName="SkipWindowsUpdates")] [switch] $SkipWindowsUpdates    
)

import-module $PSScriptRoot\wintriallab-postinstall.psm1
$errorActionPreference = "Stop"

Invoke-ScriptblockAndCatch -scriptBlock {
    Write-EventLogWrapper "Starting the autounattend postinstall script"
    Set-IdleDisplayPoweroffTime -seconds 0
    Set-PasswordExpiry -accountName "vagrant" -expirePassword $false
    Disable-HibernationFile
    Enable-MicrosoftUpdate
    
    # Need to reboot for some of these drivers to take
    # Requires that the packer file attach the Guest VM driver disc, rather than upload it 
    # (Uploading it also gives problems when using WinRM - too big? - so this is a better solution anyway)
    Install-VBoxAdditions -fromDisc 
    
    # Required for Windows 10, not required for 81, not sure about other OSes
    # Should probably happen after installing Guest VM drivers, in case installing the drivers would cause Windows to see the network as a new connection 
    Set-AllNetworksToPrivate  

    switch ($PsCmdlet.ParameterSetName) {
        "RunWindowsUpdates"  { $restartCommand = [ScriptBlock]::Create("A:\win-updates.ps1 -PostUpdateExpression A:\enable-winrm.ps1") }
        "SkipWindowsUpdates" { $restartCommand = [ScriptBlock]::Create("A:\enable-winrm.ps1") }
        default              { throw "Not configured for this parameter set..." }
    }
    Set-RestartScheduledTask -RestartCommand $restartCommand | out-null
    Restart-Computer -force 
}

