<#
Fucking Packer is giving me problems with its shell, windows-shell, and powershell provisioners, so fuck it
Don't require parameters - it won't run with parameters during post install. This is just for clarity & ease of debugging
#>
[cmdletbinding()] param(
    $packerBuildName = ${env:PACKER_BUILD_NAME},
    $packerBuilderType = ${env:PACKER_BUILDER_TYPE}
)
$errorActionPreference = "stop"
import-module $PSScriptRoot\wintriallab-postinstall.psm1
Invoke-ScriptblockAndCatch -scriptBlock {
    Write-EventLogWrapper "PostInstall for packer build '$packerBuildName' of type '$packerBuilderType'"
    Install-SevenZip
    Disable-AutoAdminLogon
    Enable-RDP
    Install-Chocolatey

    $suoParams = @{ 
        ShowHiddenFiles = $true
        ShowSystemFiles = $true
        ShowFileExtensions = $true
        ShowStatusBar = $true
        DisableSharingWizard = $true
        EnablePSOnWinX = $true
        EnableQuickEdit = $true
    }
    Set-UserOptions @suoParams

    Install-CompiledDotNetAssemblies   # Takes about 15 minutes for me 
    Compress-WindowsInstall            # Takes maybe another 15 minutes
}
