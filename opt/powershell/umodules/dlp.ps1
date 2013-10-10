# Stuff for work

$dlpCorePath = "C:\Projects\DLP\Ed-Fi-Core"
$dlpAppsPath = "C:\Projects\DLP\Ed-Fi-Apps"
function Get-DlpDeploymentFile {
    param(
        [parameter(mandatory=$true)] [string] $query,
        [validateset("core","apps","any")] [string] $location = "any"
    )
    if     ($location -eq "core") { $l = "$dlpCorePath\logistics" }
    elseif ($location -eq "apps") { $l = "$dlpAppsPath\logistics" }
    elseif ($location -eq "any")  { $l = "$dlpCorePath\logistics","$dlpAppsPath\logistics" }

    gci -recurse $l -include $query
}
set-alias gdpl Get-DlpDeploymentFile

