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
# Gotta do this outside Invoke-ScriptblockAndCatch because I need to use try/catch here: 
try { 
    Set-PinnedApplication -Action UnpinFromTaskbar -Filepath "C:\Program Files\WindowsApps\Microsoft.WindowsStore_2015.10.5.0_x86__8wekyb3d8bbwe\WinStore.Mobile.exe" -ErrorAction Continue
} 
catch {}
Invoke-ScriptblockAndCatch -scriptBlock {
    Write-EventLogWrapper "PostInstall for packer build '$packerBuildName' of type '$packerBuilderType'"
    Install-SevenZip
    Disable-AutoAdminLogon
    Enable-RDP
    Install-Chocolatey

    $suoParams = @{ 
        ShowHiddenFiles = $true
        #ShowSystemFiles = $true
        ShowFileExtensions = $true
        ShowStatusBar = $true
        DisableSharingWizard = $true
        EnablePSOnWinX = $true
        EnableQuickEdit = $true
        DisableSystrayHide = $true
        DisableIEFirstRunCustomize = $true
    }
    Set-UserOptions @suoParams

    # TODO: This would be better done as links because they're easier to deal with later
    # TODO: document the difference between using Set-PinnedApplication vs the links in AppData
    Set-PinnedApplication -Action PinToTaskbar -Filepath "$PSHOME\Powershell.exe"
    Set-PinnedApplication -Action PinToTaskbar -Filepath "${env:SystemRoot}\system32\eventvwr.msc"
    $UserPinnedTaskBar = "${env:AppData}\Microsoft\Internet Explorer\Quick Launch\User Pinned\TaskBar"
    if (test-path "$UserPinnedTaskBar\Server Manager.lnk") { rm "$UserPinnedTaskBar\Server Manager.lnk" }


    Install-CompiledDotNetAssemblies   # Takes about 15 minutes for me 
    Compress-WindowsInstall            # Takes maybe another 15 minutes
}
