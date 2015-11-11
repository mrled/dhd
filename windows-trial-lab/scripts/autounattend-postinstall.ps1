[cmdletbinding()] param()

import-module $PSScriptRoot\wintriallab-postinstall.psm1
$errorActionPreference = "Stop"

Invoke-ScriptblockAndCatch -scriptBlock {
    Write-EventLogWrapper "Starting the autounattend postinstall script"
    Set-IdleDisplayPoweroffTime -seconds 0
    Set-PasswordExpiry -accountName "vagrant" -expirePassword $false
    Disable-HibernationFile
    Enable-MicrosoftUpdate
    Set-AllNetworksToPrivate # Required for Windows 10, not required for 81, not sure about other OSes
    Install-VBoxAdditions -fromDisc # Need to reboot for some of these drivers to take
    
    Set-PinnedApplication -Action PinToTaskbar -Filepath "$PSHOME\Powershell.exe"
    Set-PinnedApplication -Action PinToTaskbar -Filepath "${env:SystemRoot}\system32\eventvwr.msc"
    
    # To reboot, then run Windows updates, then enable WinRM: 
    $winRmCommand = "$PSHome\powershell.exe -File A:\enable-winrm.ps1"
    $winUpdateCommand = "$PSHOME\powershell.exe -File A:\win-updates.ps1 -RestartAction RunAtLogon -PostUpdateExpression `"$winRmCommand`""

    # To install Windows Updates then enable WinRM after reboot:
    Set-RestartRegistryEntry -restartAction RunAtLogon -restartCommand $winUpdateCommand

    # To just enable WinRM without installing updates after reboot: 
    #Set-RestartRegistryEntry -restartAction RunAtLogon -restartCommand $winRmCommand
        
    $message = "Checking restart registry key: `r`n"
    Get-RestartRegistryEntry | select -expand StringRepr |% { $message += "`r`n$_`r`n"}
    Write-EventLogWrapper $message
    
    Restart-Computer -force 
}
