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
- Search across all checked out repositories (would be dependent on current branch checked out)
- Search across all repos for one client, or by individual client contexts
- Ability to use 'ack' for faster search (via a function or switch)
- Check out default branch named after client (so tdoe-development rather than just development)
- Change into various project contexts, which reimports path-resolver for your project
- Better doxx for these functions
- Fewer typings? E.g. this is really long: cd "$($DLPOrganizations.tdoe.Directory)/Ed-Fi-ODS/logistics" 
#>

<#
$projPaths = @("D:\Projects","C:\Projects") #This can be an array. 

$edfiBasePath = "C:\Projects\DLP"
$edfiCorePath = "C:\Projects\DLP\Ed-Fi-Core"
$edfiAppsPath = "C:\Projects\DLP\Ed-Fi-Apps"
$edfiToolsPath = "C:\Projects\DLP\Ed-Fi-Tools"


function Get-DlpProjectBasePath {
    param(
        [validateset("core","apps","all","current")] [string] $repo = "all",
        [string] $basePath = $null
    )
    if (-not $basePath) {
        $curpath = (get-item $pwd).fullname 
        foreach ($pp in @($projPaths)) {
            if ($curpath.startswith($pp)) {
                $splitPath = $curpath -split '\\'
                $basePath = $splitpath[0..2] -join '\' #this will be something like C:\Projects\DLP
                $currentRepo = $splitpath[3] # this will be something like Ed-Fi-Apps or whatever
                break
            }
        }
    }
    if (-not $basePath) {
        $basePath = $edfiBasePath
    }
    $appsPath = "$basePath\Ed-Fi-Apps"
    $corePath = "$basePath\Ed-Fi-Core"
    switch ($repo) {
        "core" { $location = "$corePath" }
        "apps" { $location = "$appsPath" }
        "all" { $location = "$corePath","$appsPath" }
        "current" { $location = "$basePath\$currentRepo"}
    }
    return get-item $location
}

function Get-DlpProjectFile {
    [CmdletBinding(DefaultParametersetName="QueryType")] 
    param(
        [Parameter(ParameterSetName='Query',Position=0)] [alias("query")] [string[]] $include,
        [Parameter(ParameterSetName='Query')] [string[]] $exclude,
        [Parameter(ParameterSetName='Query')] [string[]] $subdir,

        [Parameter(ParameterSetName='QueryType')] 
        [validateset('logistics','source','visualstudio','database','etl','alltypes','allfiles')] 
        [string[]] $type = 'logistics',

        [string] $containingPattern,
        [switch] $caseSensitive,

        [validateset("core","apps","all","current")] [string] $repo = "all",
        [string] $basePath = $null
    )
    $searchBase = Get-DlpProjectBasePath -repo $repo -basePath $basePath

    # A bunch of predefined query types, to match the -type parameter. Just maakes things convenient.
    $qtypes = @{}
    $qtypes.logistics = @{}
    $qtypes.logistics.include = @('*.ps1','*.psm1','*.psd1','credentials-*.xml')
    $qtypes.logistics.subdir = @('logistics','src\SolutionScripts')
    $qtypes.source = @{}
    $qtypes.source.include = @('*.cs')
    $qtypes.source.subdir = @('Application','src')
    $qtypes.visualstudio = @{}
    $qtypes.visualstudio.include = @('*.sln','*.csproj','*.ccproj','*.config','*.cscfg','*.cspkg','*.csdef')
    $qtypes.visualstudio.subdir = @('Application','src')
    $qtypes.config = @{}
    $qtypes.config.include = @('*.config')
    $qtypes.config.subdir = @('Application','src')
    $qtypes.database = @{}
    $qtypes.database.include = @('*.sql')
    $qtypes.database.subdir = @('Database')
    $qtypes.etl = @{}
    $qtypes.etl.include = @('*.dtsx')
    $qtypes.etl.subdir = @('Etl')
    $qtypes.alltypes = @{}
    $qtypes.alltypes.include = foreach ($k in $qtypes.keys) { $qtypes[$k]['include'] } 
    $qtypes.alltypes.subdir = foreach ($k in $qtypes.keys) { $qtypes[$k]['subdir'] }
    $qtypes.allfiles = @{}
    $qtypes.allfiles.include = ''
    $qtypes.allfiles.subdir = '.'

    if ($PsCmdlet.ParameterSetName -eq 'QueryType') {
        $include = $qtypes["$type"]['include']
        $subdir = $qtypes["$type"]['subdir'] 
    }

    $expandedLocations = @()
    foreach ($sd in @($subdir)) {
        foreach ($sb in @($searchBase)) {
            if (test-path "$sb\$sd") {
                $expandedLocations += "$sb\$sd"
            }
        }
    }

    $verbMessage = "Looking in '$expandedLocations' for files like '$include'"
    if ($exclude) { $verbMessage += " but not like '$exclude'"}
    write-verbose $verbMessage
    $allMatches = gci -recurse $expandedLocations -include $include -exclude $exclude
    if ($PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent) { 
        write-verbose "Matches:"
        foreach ($match in $allmatches) {
            write-verbose $match.name
        }
    }
    if ($containingPattern) {
        $slsParms = @{
            quiet = $true
            pattern = $containingPattern
            CaseSensitive = $caseSensitive.ispresent
        }
        write-verbose "Searching through results files for strings matching '$containingPattern'..."
        $results = $allMatches |? { $_ | sls @slsparms }
    }
    else {
        $results = $allMatches
    }
    return $results
}
set-alias gdlp Get-DlpProjectFile

function Set-DLPProjectContext {
    param(
        [parameter(mandatory=$true)] [ValidateSet("rest","dash","old")] $context
    )
    switch ($context) {
        "rest" {
            $env:PathResolverRepositoryOverride = "TDOE-RestApiSpike"
        }
        "dash" {
            $env:PathResolverRepositoryOverride = "Ed-Fi-Common;Ed-Fi-Dashboards-Core;Ed-Fi-Apps"
        }
        "old" {
            $env:PathResolverRepositoryOverride = "Ed-Fi-Core;Ed-Fi-Apps"
        }
    }
    $LoadedPathResolver = get-module | where { $_.Name -eq "path-resolver" }
    if ($LoadedPathResolver) {
        remove-module path-resolver
    }
    ipmo "$edfiBasePath\TDOE-RestApiSpike\logistics\scripts\modules\path-resolver.psm1"
}
set-alias setdlp Set-DLPProjectContext
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

function Get-ODSConfigTransforms {
    [cmdletbinding(DefaultParametersetName="env")] param(
        [parameter(mandatory=$true,position=0)] [string] $client,
        [parameter(ParameterSetName="env")] [string] $environmentName,
        [parameter(ParameterSetName="allenvs")] [switch] $all,
        [parameter(ParameterSetName="proj")] [switch] $project
    )

    $clientPath = resolve-path "$dlpProjectDrive\$client"

    $fuckingProjects = @(
        "EdFi.Ods.Admin.Web"
        "EdFi.Ods.BulkLoad.Console"
        "EdFi.Ods.SwaggerUI"
        "EdFi.Ods.WebApi"
        "EdFi.Ods.BulkLoad.Services.Windows.BulkWorker"
        "EdFi.Ods.BulkLoad.Services.Windows.UploadWorker"
    )

    $excludes = ""
    switch ($pscmdlet.ParameterSetName) {
        "env" { 
            if ($environmentName) { $includes = "*.${environmentName}.config" }
            else {                  $includes = "App.config","Web.config" }
        }
        "allenvs" { 
            $includes = "*.config"
            $excludes = "packages.config" 
        }
        "proj" { 
            $includes = "*proj" 
        }
    }

    foreach ($proj in $fuckingProjects) {
        $projPath = resolve-path "$clientPath\Ed-Fi-ODS-Implementation\Application\$proj"
        $paths = @("$projPath\*")
        if (test-path $projPath\AzureStartup) {
            $paths += @("$projPath\AzureStartup\*")
        }
        gci -path $paths -include $includes -exclude $excludes
    }
}

function New-ODSConfigFiles {
    [cmdletbinding()] param(
        [parameter(mandatory=$true,position=0)] [string] $client,
        [parameter(mandatory=$true)] [string] $environmentName,
        [string] $fromEnvironment = "Example",
        [switch] $force
    )
    $newFiles = @()
    foreach ($example in (Get-ODSConfigTransforms -client $client -environmentName "$fromEnvironment")) {
        $newName = $example.Name -replace "$fromEnvironment","$environmentName"
        $newPath = "$($example.Directory.Fullname)\$newName"
        $newFiles += @(copy-item $example $newPath -passthru -force:$force)
    }

    $buildActivity = resolve-path "$dlpProjectDrive\$client\Ed-Fi-ODS-Implementation\logistics\scripts\activities\build"
    $newFiles += @(new-item -ItemType file -Path "$buildActivity\$environmentName.vars.ps1" -force:$force)
    $newFiles += @(new-item -ItemType file -Path "$buildActivity\credentials-$environmentName.xml" -force:$force)

    return $newFiles
}
