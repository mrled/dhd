[cmdletbinding()] param()

import-module $PSScriptRoot\wintriallab-postinstall.psm1

Invoke-ScriptblockAndCatch -scriptBlock {
    Write-EventLogWrapper "Starting the autounattend postinstall script"
    Set-IdleDisplayPoweroffTime -seconds 0
    Set-PasswordExpiry -accountName "vagrant" -expirePassword $false
    Disable-HibernationFile
    Enable-MicrosoftUpdate
    Set-AllNetworksToPrivate # Required for Windows 10, not required for 81, not sure about other OSes
    Install-VBoxAdditions -fromDisc # Need to reboot for some of these drivers to take
    
    # To reboot, then run Windows updates, then enable WinRM: 
    $winRmCommand = "$PSHome\powershell.exe -File A:\enable-winrm.ps1"
    $winUpdateCpmmand = "$PSHOME\powershell.exe -File A:\win-updates.ps1 -CalledFromRegistry -RestartAction RunAtLogon -PostUpdateExpression `"$winRmCommand`""
        
    # To install Windows Updates then enable WinRM after reboot:
    Set-RestartRegistryEntry -restartAction RunAtLogon -restartCommand $winUpdateCpmmand

    # To just enable WinRM without installing updates after reboot: 
    #Set-RestartRegistryEntry -restartAction RunAtLogon -restartCommand $winRmCommand
        
    Restart-Computer -force 
 }
