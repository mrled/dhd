# Stuff for work

$dlpumodule = $myinvocation.mycommand.path

$edfiBasePath = "C:\Projects\DLP"
$edfiCorePath = "C:\Projects\DLP\Ed-Fi-Core"
$edfiAppsPath = "C:\Projects\DLP\Ed-Fi-Apps"
$edfiToolsPath = "C:\Projects\DLP\Ed-Fi-Tools"

function Get-DlpProjectFile {
    param(
        [alias("include")] [string[]] $query = @("*.ps1","*.psm1"),
        [string[]] $exclude = $null,
        [validateset("core","apps","any")] [string] $repo = "any",
        [string[]] $subdir = "logistics",
        [string[]] $containing
    )
    if     ($repo -eq "core") { $l = "$edfiCorePath\$subdir" }
    elseif ($repo -eq "apps") { $l = "$edfiAppsPath\$subdir" }
    elseif ($repo -eq "any")  { $l = "$edfiCorePath\$subdir","$edfiAppsPath\$subdir" }

    $r = gci -recurse $l -include $query -exclude $exclude
    if ($containing) {
        $results = $r |? { $_ | sls -quiet -pattern $containing }
    }
    else {
        $results = $r
    }
    return $results
}
set-alias gdlp Get-DlpProjectFile

function Convert-OpenSSLPemToPfx {
    param(
        [parameter(mandatory=$true)] [string] $pemfile,
        [string] $displayname
    )
    $basename = split-path -leaf $pemfile
    if (-not $displayname) {
        $displayname = $basename
    }
    openssl pkcs12 -export -out "$basename.pfx" -in "$pemfile" -name "$displayname"
}

function Generate-DlpCredentialCertificate {
    param(
        [parameter(mandatory=$true)] [string] $certName,
        [int] $keySize = 4096,
        [int] $daysValid = 7300
    )
    openssl req -x509 -nodes -days $daysValid -subj "/CN=$certName" -newkey rsa:$keysize -keyout "$certName.pem" -out "$certName.pem"
    Convert-OpenSSLPemToPfx -pemfile "$certName.pem"
}

function Add-DlpClientGitRemote {
    <#
    Assumes repos named standard names (Ed-Fi-Core), checked out to standard places (C:\Projects\DLP\Ed-Fi-Core)
    #>
    param(
        [parameter(mandatory=$true)] [string] $orgName,
        [string] $projectsPath = "$edfiBasePath",
        [string[]] $repoNames = @("Ed-Fi-Core","Ed-Fi-Apps"),
        [string] $branchName = "master"
    )
    foreach ($repoName in $repoNames) {
        cd "$projectsPath\$repoName"
        git remote add $orgName "git@github.com:$orgname/$repoName.git"
        git fetch $orgName
        git checkout -b "$orgName-$branchName" "$orgName/$branchName"
    }
}
set-alias adlpclient Add-DlpClientGitRemote