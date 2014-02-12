# Stuff for work

$dlpumodule = $myinvocation.mycommand.path

$projPaths = @("D:\Projects") #This can be an array

$edfiBasePath = "D:\Projects\DLP"
$edfiCorePath = "D:\Projects\DLP\Ed-Fi-Core"
$edfiAppsPath = "D:\Projects\DLP\Ed-Fi-Apps"
$edfiToolsPath = "D:\Projects\DLP\Ed-Fi-Tools"

function Get-DlpProjectFile {
    [CmdletBinding(DefaultParametersetName="Query")] 
    param(
        [Parameter(ParameterSetName='Query',Position=0)] [alias("include")] [string[]] $query = @("*.ps1","*.psm1"),
        [Parameter(ParameterSetName='Query')] [string[]] $exclude = $null,
        [Parameter(ParameterSetName='Query')] [string[]] $subdir = "logistics",
        [Parameter(ParameterSetName='Query')] [string] $containingPattern,
        [Parameter(ParameterSetName='Query')] [switch] $caseSensitive,
        [Parameter(ParameterSetName='GetBase')] [switch] $getBase,
        [validateset("core","apps","all")] [string] $repo = "all",
        [string] $basePath = $null
    )
    if (-not $basePath) {
        $curpath = (get-item $pwd).fullname 
        foreach ($pp in @($projPaths)) {
            if ($curpath.startswith($pp)) {
                $basePath = ($curpath -split '\\')[0..2] -join '\'
                break
            }
        }
    }
    if (-not $basePath) {
        $basePath = $edfiBasePath
    }
    $appsPath = "$basePath\Ed-Fi-Apps"
    $corePath = "$basePath\Ed-Fi-Core"

    switch ($PsCmdlet.ParameterSetName) {
        'Query' {
            if     ($repo -eq "core") { $location = "$corePath\$subdir" }
            elseif ($repo -eq "apps") { $location = "$appsPath\$subdir" }
            elseif ($repo -eq "all")  { $location = "$corePath\$subdir","$appsPath\$subdir" }
            $r = gci -recurse $location -include $query -exclude $exclude
            if ($containingPattern) {
                $slsParms = @{
                    quiet = $true
                    pattern = $containingPattern
                    CaseSensitive = $caseSensitive.ispresent
                }
                $results = $r |? { $_ | sls @slsparms }
            }
            else {
                $results = $r
            }
        }
        'GetBase' {
            if     ($repo -eq "core") { $location = "$corePath" }
            elseif ($repo -eq "apps") { $location = "$appsPath" }
            elseif ($repo -eq "all")  { $location = "$corePath","$appsPath" }
            $results = get-item $location
        }
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
    Assumes repos named standard names (Ed-Fi-Core), checked out to standard places (F:\Projects\DLP\Ed-Fi-Core)
    #>
    param(
        [parameter(mandatory=$true)] [string] $orgName,
        [string] $basePath = "$edfiBasePath",
        [string[]] $repoNames = @("Ed-Fi-Core","Ed-Fi-Apps"),
        [string] $branchName = "master",
        [string] $remoteName # useful so that the "DoubleLinePartners-MSDF" github org can be referred to as simply "msdf" in `git remote`.
    )
    foreach ($repoName in $repoNames) {
        cd "$basePath\$repoName"
        if (-not $remoteName) {
            $remoteName = $orgName
        }
        git remote add $remoteName "git@github.com:$orgname/$repoName.git"
        git fetch $remoteName
        git checkout -b "$remoteName-$branchName" "$remoteName/$branchName"
    }
}
set-alias adlpclient Add-DlpClientGitRemote

function Invoke-GitCommandOnDlpRepositories {
    param(
        [Parameter(mandatory=$true)] [string] $gitCommand,
        [string] $basePath,
        [validateset("core","apps","all")] [string] $repo = "all"
    )
    $oldpath = get-location
    foreach ($path in (Get-DlpProjectFile -getbase -repo $repo -basepath $basepath)) {
        set-location $path | out-null
        write-host -foreground magenta $path.fullname
        invoke-expression "git $gitCommand"
    }
    set-location $oldpath
}
set-alias dgit Invoke-GitCommandOnDlpRepositories

