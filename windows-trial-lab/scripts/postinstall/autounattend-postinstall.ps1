[cmdletbinding()]
param()

import-module $PSScriptRoot\wintriallab-postinstall.psm1

try {
    Write-EventLogWrapper "Starting the autounattend postinstall script"
    Set-PasswordExpiry -accountName "vagrant" -expirePassword $false
    Disable-HibernationFile
    Enable-MicrosoftUpdate
    Install-VBoxAdditions -fromDisc # Need to reboot for some of these drivers to take
    
    # To reboot, then run Windows updates, then enable WinRM: 
    $restartCommand = "$PSHOME\powershell.exe -File A:\win-updates.ps1 -RestartAction RunAtLogon -PostUpdateExpression -CalledFromRegistry '$PSHOME\powershell.exe -File A:\enable-winrm.ps1'"
    
    # To reboot, then run winrm immediately without Windows Update
    #$restartCommand = "$PSHOME\powershell.exe -File A:\enable-winrm.ps1"
    
    Set-RestartRegistryEntry -restartAction RunAtLogon -restartCommand $restartCommand    
    Restart-Computer -force 
 }
catch {
    $message  = "======== CAUGHT EXCEPTION ========`r`n$_`r`n"
    $message += "======== ERROR STACK ========"
    $error |% { $message += "$_`r`n----`r`n" }
    $message += "======== ========"
    Write-EventLogWrapper $message
    exit 666
}
