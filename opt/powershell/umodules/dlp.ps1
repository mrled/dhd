$dlpProfile = resolve-path $PSScriptRoot\..\profile.d\dlp.ps1
$DLPProjectBase = "~/Documents/DLPClients"
if (test-path $DLPProjectBase) {
    $DLPProjectBase = (resolve-path $DLPProjectBase).Path
    . $dlpProfile
}

