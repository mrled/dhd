<#
.synopsis
Postinstall script for my iso updater Vagrant boxes
.notes
- Enable current user autologon
- Install the Windows ADK
- Add the win-updates.ps1 task to run 1 minute from now. That task should call trial-iso-updater.ps1 when finished.
- Exit
TODO: can I pull the Vagrant username/password from the environment somehow? Sucks to have it hardcoded
#>

import-module $PSScriptRoot\wintriallab-postinstall.psm1

Invoke-ScriptblockAndCatch -scriptBlock {

    Install-Chocolatey
    choco install --yes --force windows-adk

    $restartCommandString = '& "{0}\win-updates.ps1" -MaxCycles 5 -PostUpdateExpression "{0}\trial-iso-updater.ps1"' -f $PSScriptRoot
    $restartCommandSb = [ScriptBlock]::Create($restartCommandString)
    Set-RestartScheduledTask -RestartCommand $restartCommandSb

    Set-AutoAdminLogon -Username "vagrant" -Password "V@grant123"

    # This returns immediately, which means Vagrant's provisioner will hopefully not interpret the restart as a failure? 
    shutdown.exe /r /f /t 10 /d u:0:0 /c "Reboot to run win-updates.ps1"

}
