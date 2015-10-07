<#
Fucking Packer is giving me problems with its shell, windows-shell, and powershell provisioners, so fuck it
Don't require parameters - it won't run with parameters during post install. This is just for clarity & ease of debugging
#>
[cmdletbinding()] 
param(
    $packerBuildName = ${env:PACKER_BUILD_NAME},
    $packerBuilderType = ${env:PACKER_BUILDER_TYPE},
    $tempDir # calculated later on if this is empty
)

$errorActionPreference = "stop"

write-verbose "PostInstall for packer build '$packerBuildName' of type '$packerBuilderType'"

if ($packerBuilderType -notmatch "virtualbox") {
    $warning = "@@@WARNING@@@ I have no way to install tools for your selected Packer build type of '$packerBuilderType'"
    write-host -foreground red -object $warning
}

$LASTEXITCODE = 0 # just in case 

import-module $PSScriptRoot\wintriallab-postinstall.psm1
try {
    Install-SevenZip
    #Install-VBoxAdditions -isoPath "C:\Users\vagrant\VBoxGuestAdditions.iso"
    Install-VBoxAdditions -fromDisc
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
catch {
    write-host "======== CAUGHT EXCEPTION ========"
    write-host "$_"
    write-host "======== CALL STACK ========"
    Get-PSCallStack | format-list
    write-host "======== ERROR STACK ========"
	for ($i=0; $i<$error.count; $i+=1) {
		write-host "`$error[$i]"
		write-host $error[$i]
	}
    write-host "======== ========"
    exit 666
}
