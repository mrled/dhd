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

$ipfd = @{}
gci "$dlpProjectDrive\" |% { 
    $client = $_.name
    $ipfdPath = "$($_.fullname)\Ed-Fi-ODS-Implementation\Initialize-PowershellForDevelopment.ps1"
    if (test-path $ipfdPath) {
        $ipfd[$client] = $ipfdPath
    }
}

ipmo DLPHelper
ipmo "$DLPProjectBase\dlp\InternalTools\encrypted-credentials"

<#
Functionality I'd like to have: 
- Change into various project contexts, which reimports path-resolver for your project
#>

function Generate-DlpCredentialCertificate {
    param(
        [parameter(mandatory=$true)] [string] $certName,
        [parameter(mandatory=$true)] [string] $pfxPassword,
        [int] $keySize = 4096,
        [int] $daysValid = 7300
    )
    Invoke-OpenSsl @( 'req', '-x509', '-nodes', '-days', "$daysValid", "-subj", "`"/CN=$certName`"", 
        "-newkey", "rsa:$keysize", "-keyout", "`"$certName.pem`"", "-out", "`"$certName.pem`"")
    Convert-OpenSSLPemToPfx -pemfile "$certName.pem" -pfxPassword $pfxPassword
}

function Invoke-GitCommandOnMultipleRepositories {
    param(
        [Parameter(mandatory=$true)] [string] $gitCommand,
        [Parameter(mandatory=$true)] [array] $repositoryList
    )
    $resolvedRepositoryList = @()
    foreach ($repo in $repositoryList) {
        $resolvedRepositoryList += @(resolve-path $repo)
    }

    $oldpath = get-location
    foreach ($repoLocation in $resolvedRepositoryList) {
        set-location $repoLocation | out-null
        write-host -foreground magenta $repoLocation
        invoke-expression "git $gitCommand"
    }
    set-location $oldpath
}
set-alias mgit Invoke-GitCommandOnMultipleRepositories

function Connect-DLPFlameleafVPN {
    $DomainCreds = Get-DLPProtectedCredential -credentialFile ~/credentials-MRLDLP.xml -credentialName "micah@doubleline.us"
    ipsecc -a -r FlameLeafVPN.vpn -u $DomainCreds.Username -p "$($DomainCreds.DecryptPassword())"
}

function Delete-TeamCityBuild {
    param(
        $teamcityhost, 
        $build, 
        $username, 
        $password
    )
    $encodedcredentials = [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($username+":"+$password))
    $buildUri = "$teamcityhost/httpAuth/app/rest/builds/id:$build"
    $headers = @{"Authorization"="Basic $encodedcredentials"}
    Invoke-WebRequest -Uri $buildUri -Method Delete -Headers $headers
}

function New-SelfSignedCert {
    param(
        [parameter(mandatory=$true)] [string[]] $certName,
        [parameter(mandatory=$true)] [string[]] $pfxPassword
    )
    if ($certName.count -ne $pfxPassword.count) {
        throw "Must pass the same number of -certName and -pfxPassword arguments"
    }

    $outLines = @()
    for ($i=0; $i -lt $certName.count; $i+=1) {
        $cn = $certName[$i]
        $ppass = $pfxPassword[$i]
        minipki selfsign $cn
        $cf = resolve-path ".\certified-keys\${cn}.crt"
        $kf = resolve-path ".\private\${cn}.key"
        Convert-OpenSSLPemToPfx -certFile $cf -keyFile $kf -pfxPassword $ppass

        $pf = resolve-path ".\certified-keys\${cn}.pfx"
        $thumb = (Get-OpenSSLThumbprint -pemFile $cf) -replace "`n",""
        $outLines += @("$thumb :: $($pf.path)")
    }

    write-host "--------"
    $outLines |% { write-output $_ }
}


