<#
ASSUMPTIONS: 
- $DLPProjectBase must be set to an existing directory (this is done in umodules/dlp.ps1)
- You must have micah's DLPLogisticsModules checked out to $DLPProjectBase/misc/DLPLogisticsModules
#>

if (-not ($VisualStudioDirectories -contains $DLPProjectBase)) {
    $VisualStudioDirectories += @($DLPProjectBase)
}

function New-DlpProject {
    param(
        [parameter(mandatory=$true)] [string] $GitHubOrg,
        [string] $LocalName,
        [switch] $Checkout,
        [string[]] $Repositories,
        [hashtable] $Contexts
    )
    $project = New-Object PSObject
    Add-Member -InputObject $project -NotePropertyMembers  @{
        _ExplicitRepos = $Repositories
        _ExplicitCheckout = $Checkout
        LocalName = if ($LocalName) { $Localname } else { $GitHubOrg }
        GitHubOrg = $GitHubOrg
        Contexts = $Contexts
    }
    # $project.Checkout should be true if -Checkout is passed or if there are any -Contexts specified
    Add-Member -InputObject $project -MemberType ScriptProperty -Name Checkout -Value {
        return $this._ExplicitCheckout -or [bool]$this.Contexts.count
    }
    # $project.Repositories contains all items passed to -Repositories and all repos set in values of the -Contexts hashtable
    Add-Member -InputObject $project -MemberType ScriptProperty -Name Repositories -Value {
        $repos = @()
        $repos += $this._ExplicitRepos
        foreach ($ctx in $this.Contexts.keys) {
            foreach ($repo in $this.Contexts[$ctx]) {
                $repos += $repo
            }
        }
        $repos | sort -uniq
    }
    Add-Member -InputObject $project -MemberType ScriptProperty -Name Directory -Value {
        $projDir = "$DLPProjectBase\$($this.Localname)"
        if (-not $this.checkout) {
            return ""
        }
        elseif (test-path $projDir) {
            return get-item $projDir
        }
        else {
            return "$projDir"
        }
    }
    <#
    Add-Member -InputObject $project -MemberType ScriptMethod -Name SerializeConstructorParameters -Value {
        return @{
            GitHubOrg = $this.GitHubOrg
            LocalName = $this.LocalName
            Checkout = $this._ExplicitCheckout
            Repositories = $this._ExplicitRepos
            Contexts = $this.Contexts
        }
    }
    #>
    return $project
}

ipmo "$DLPProjectBase\dlp\InternalTools\encrypted-credentials"

$DLPOrganizations = @{}
@(
    New-DlpProject -LocalName mrled -GitHubOrg mrled -Checkout `
        -Repositories @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Tools','Ed-Fi-Dashboards-Core',
            'Ed-Fi-ODS','Ed-Fi-ODS-Implementation','Ed-Fi-Common','minipki') 
    New-DlpProject -LocalName dlp -GitHubOrg DoubleLinePartners -Checkout -Repositories @('InternalTools')
    New-DlpProject -LocalName alliance -GitHubOrg 'Ed-Fi-Alliance' -Checkout `
        -Contexts @{ rest = @('Ed-Fi-Common','Ed-Fi-Standard','Ed-Fi-ODS','Ed-Fi-ODS-Implementation') } `
        -Repositories @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Dashboards-Core') 
    New-DlpProject -LocalName msdf -GitHubOrg 'DoubleLinePartners-MSDF' -repositories @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Common') 
    New-DlpProject -LocalName edfiteam -GitHubOrg 'DoubleLinePartners-Ed-FiTeam' -Contexts @{
        dash = @('Ed-Fi-Core','Ed-Fi-Apps') 
    }
    New-DlpProject -LocalName tdoe -GitHubOrg 'TennesseeDOE' `
        -Contexts @{ 
            rest = @('Ed-Fi-Common','Ed-Fi-ODS','Ed-Fi-ODS-Implementation') 
            dash = @('Ed-Fi-Common','Ed-Fi-Dashboards-Core','Ed-Fi-Apps')
        }
    #New-DlpProject -LocalName tea -GitHubOrg 'TexasEA' `
    #    -Contexts @{ 
    #        dash = @('Ed-Fi-Core','Ed-Fi-Apps')
    #    }
    New-DlpProject -LocalName nedoe -GitHubOrg NebraskaDOE `
        -Contexts @{ 
            rest = @('Ed-Fi-Common','Ed-Fi-ODS') 
            dash = @('Ed-Fi-Common','Ed-Fi-Core','Ed-Fi-Apps')
        }
    New-DlpProject -LocalName pde -GitHubOrg PennsylvaniaDOE -Contexts @{ dash = @('Ed-Fi-Common','Ed-Fi-Core','Ed-Fi-Apps') } `
) |% { $DLPOrganizations[$_.LocalName] = $_ }

$clientUpdateSb = {
    # Note: job script blocks don't work very well with custom objects
    # I was having a problem where some members were present, but others, such as ScriptProperty members,
    # were not present when the object was passed into the script block
    # Job SBs also cannot use externally defined functions such as New-DlpProject or In-CliXml
    param(
        [parameter(mandatory=$true)] $clientName,
        [parameter(mandatory=$true)] $serializedDlpOrganizations,
        [string] $gitPath,
        [bool] $WhatIf = $false
    )
    $DlpOrganizations = [System.Management.Automation.PSSerializer]::Deserialize($serializedDlpOrganizations)
    $clientOrganization = $DLPOrganizations[$clientName]
    $localName = $clientOrganization.localName
    $GitHubOrg = $clientOrganization.GitHubOrg
    $projectDir = $clientOrganization.Directory
    if ($gitPath) {
        set-alias GitExe $gitPath
    }
    else {
        set-alias GitExe "${env:ProgramFiles(x86)}\Git\bin\git.exe"
    }
    write-output "==== Updating client '$clientName'... ===="
    if (-not $clientOrganization.Checkout) {
        write-output "Client '$LocalName' was not set to be checked out."
    }
    else {
        if (-not (test-path $projectDir)) {
            mkdir $projectDir | out-null
        }
        foreach ($repoName in $clientOrganization.Repositories) {
            set-location $projectDir 
            # $repoClients is all the organizations that contain $repoName
            $repoClients = ($DLPOrganizations.values |? { $_.Repositories -contains $repoName }).LocalName
            if (-not (test-path "$projectDir\$repoName")) {
                write-output "  Doing initial clone for '$repoName' for project '$localName'"
                if (-not $WhatIf) {
                    GitExe clone --origin "$localName" "git@github.com:$GitHubOrg/$repoName"
                }
            }
            else {
                write-output "  Updating repository '$repoName' for project '$localName'"
            }
            set-location $projectDir\$repoName
            foreach ($gitRemoteName in $repoClients) {
                if ((GitExe remote) -notcontains $gitRemoteName) {
                    write-output "    Adding '$gitRemoteName' remote for '$repoName' repository"
                    $ghoName = ($DLPOrganizations.values |? { $_.LocalName -match "^$gitRemoteName$" }).GitHubOrg
                    if (-not $WhatIf) {
                        GitExe remote add $gitRemoteName "git@github.com:$ghoName/$repoName"
                    }
                }
                else {
                    write-output "    Found '$gitRemoteName' remote for '$repoName' repository"
                }
            }
            write-output "  Doing 'git fetch' in repository '$repoName' for project '$localName'"
            if (-not $Whatif) {
                GitExe fetch --all
            }
        }
        write-output "==== Finished with client '$clientName'... ===="
    }
}

function Update-DlpGitProject {
    [CmdletBinding()]
    param(
        [string[]] $clientList
    )

    $workingClientList = @()
    foreach ($client in $clientList) {
        if (-not $DLPOrganizations.$client) {
            throw "Passed client name '$client', but no such client exists in `$DLPOrganizations"
        }
        $workingClientList += @($DLPOrganizations.$client)
    }
    if (-not $clientList) {
        $workingClientList = $DLPOrganizations.values
    }

    $serializedDlpOrganizations = Out-CliXml $DlpOrganizations -depth 4
    foreach ($client in $workingClientList) {
        start-job -name "git-$($client.LocalName)" -scriptBlock $clientUpdateSb -argumentList $client.LocalName,$serializedDlpOrganizations
    }
}

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
