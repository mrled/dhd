$DLPProjectBase = "~/Documents/DLPClients"
if (test-path $DLPProjectBase) {
    $DLPProjectBase = (resolve-path $DLPProjectBase).Path

    <#
    ASSUMPTIONS: 
    - $DLPProjectBase must be set to an existing directory (this is done in umodules/dlp.ps1)
    - You must have micah's DLPLogisticsModules checked out to $DLPProjectBase/misc/DLPLogisticsModules
    #>

    # I'm setting this in my Startup folder also
    # However, drives mapped with subst are only valid for the context in which they were created
    # If they were created unelevated, they will only be available to unelevated process
    $dlpProjectDrive = "P:"
    if (-not (get-psdrive |? { $_.name -eq "P" })) {
        subst $dlpProjectDrive $DLPProjectBase
        get-psdrive | out-null # if you don't do this, Powershell won't see the new drive
    }

    if (-not ($VisualStudioDirectories -contains $DLPProjectBase)) {
        $VisualStudioDirectories += @($DLPProjectBase)
    }
    if (-not ($VisualStudioDirectories -contains $dlpProjectDrive)) {
        $VisualStudioDirectories += @($dlpProjectDrive)
    }

    ipmo DLPHelper
    ipmo "$DLPProjectBase\misc\InternalTools\encrypted-credentials"
}
