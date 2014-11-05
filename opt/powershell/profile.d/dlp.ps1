<#
ASSUMPTIONS: 
- $DLPProjectBase must be set to an existing directory (this is done in umodules/dlp.ps1)
- You must have micah's DLPLogisticsModules checked out to $DLPProjectBase/misc/DLPLogisticsModules
#>

if (-not ($VisualStudioDirectories -contains $DLPProjectBase)) {
    $VisualStudioDirectories += @($DLPProjectBase)
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
