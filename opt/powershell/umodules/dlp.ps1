# Stuff for work

$DLPProjectBase = "~/Documents/DLPClients"
if (-not (test-path $DLPProjectBase)) {
    # Do nothing
}
else {
    $DLPProjectBase = resolve-path $DLPProjectBase

    $dlpumodule = $myinvocation.mycommand.path
    
    $projPaths = @("D:\Projects","C:\Projects") #This can be an array. 
    
    $edfiBasePath = "C:\Projects\DLP"
    $edfiCorePath = "C:\Projects\DLP\Ed-Fi-Core"
    $edfiAppsPath = "C:\Projects\DLP\Ed-Fi-Apps"
    $edfiToolsPath = "C:\Projects\DLP\Ed-Fi-Tools"
    
    $DLPOrganizations = @(
        @{ 
            LocalName = 'mrled'; GitHubOrg = 'mrled'; 
            Repositories = @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Tools','Ed-Fi-Dashboards-Core','Ed-Fi-ODS','Ed-Fi-Common') 
        },
        @{ 
            LocalName = 'alliance'; GitHubOrg = 'Ed-Fi-Alliance' 
            Repositories = @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Dashboards-Core','Ed-Fi-ODS','Ed-Fi-Common') 
        },
        @{ 
            LocalName = 'msdf'; GitHubOrg = 'DoubleLinePartners-MSDF' 
            Repositories = @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Common') 
        },
        @{ 
            LocalName = 'tdoe'; GitHubOrg = 'TennesseeDOE'; Checkout = $true 
            Repositories = @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Dashboards-Core','Ed-Fi-ODS','Ed-Fi-Common') 
        },
        @{ 
            LocalName = 'ndoe'; GitHubOrg = 'NebraskaDOE'; Checkout = $true 
            Repositories = @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Dashboards-Core','Ed-Fi-ODS','Ed-Fi-Common') 
        },
        @{ 
            LocalName = 'pde'; GitHubOrg = 'PennsylvaniaDOE'; Checkout = $true 
            Repositories = @('Ed-Fi-Apps','Ed-Fi-Core','Ed-Fi-Common') 
        }
    )
    
    function Update-DlpGitProject {
        [CmdletBinding()]
        param(
            [array] $clientList
        )
    
        $workingClientList = @()
        foreach ($client in $clientList) {
            $matchedClient = $DLPOrganizations |? { $_.LocalName -match $client}
            if ($matchedClient) {
                $workingClientList += @($matchedClient)
            }
            else {
                throw "Passed client '$client', but no such client exists in `$DLPOrganizations"
            }
        }
        if (-not $workingClientList) {
            $workingClientList = $DLPOrganizations |? { $_.Checkout } 
        }
    
        foreach ($client in $workingClientList) {
            write-verbose "Updating client '$($client.LocalName)'"
            $projectDir = "$DLPProjectBase\$($client.LocalName)"
            #$projectRepos = $client.Repositories
            if (-not (test-path $projectDir)) {
                mkdir $projectDir | out-null
            }
            foreach ($repoName in $client.Repositories) {
                set-location $projectDir 
                #$repoName = $repository.RepoName
                # $repoClients is all the organizations that contain $repoName
                $repoClients = ($DLPOrganizations |? { $_.Repositories -contains $repoName }).LocalName
                if (-not (test-path "$projectDir\$repoName")) {
                    write-verbose "  Doing initial clone for '$repoName' for project '$($client.LocalName)'"
                    git clone --origin "$($client.LocalName)" "git@github.com:$($client.GitHubOrg)/$repoName"
                }
                else {
                    write-verbose "  Updating repository '$repoName' for project '$($client.LocalName)'"
                }
                set-location $projectDir\$repoName
                #foreach ($gitRemoteName in $repository.Clients) {
                foreach ($gitRemoteName in $repoClients) {
                    if ((git remote) -notcontains $gitRemoteName) {
                        write-verbose "    Adding '$gitRemoteName' remote for '$repoName' repository"
                        $ghoName = ($DLPOrganizations |? { $_.LocalName -match $gitRemoteName }).GitHubOrg
                        git remote add $gitRemoteName "git@github.com:$ghoName/$repoName"
                    }
                    else {
                        write-verbose "    Found '$gitRemoteName' remote for '$repoName' repository"
                    }
                }
                write-verbose "  Doing 'git fetch' in repository '$repoName' for project '$($client.LocalName)'"
                git fetch --all
            }
        }
    }
    
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
    
    function Decrypt-SecureString {
        param(
            [Parameter(ValueFromPipeline=$true,Mandatory=$true,Position=0)] [System.Security.SecureString] $secureString
        )
        $marshal = [System.Runtime.InteropServices.Marshal]
        $pointer = $marshal::SecureStringToBSTR($secureString)
        $decryptedString = $marshal::PtrToStringBSTR($pointer)
        $marshal::ZeroFreeBSTR($pointer)
        return $decryptedString
    }
    
    function Convert-OpenSSLPemToPfx {
        param(
            [parameter(mandatory=$true)] [string] $certFile,
            [string] $keyFile = ((resolve-path $certFile).path -replace '(\.pem$)|(\.crt$)|(\.cert$)','.key'),
            $pfxPassword,
            [string] $outfile = ((resolve-path $certFile).path -replace '(\.pem$)|(\.crt$)|(\.cert$)','.pfx'),
            [string] $displayname = ((split-path -leaf $certFile) -replace '(\.pem$)|(\.crt$)|(\.cert$)','')
        )
        if (-not $pfxPassword) {
            $securePfxPassword = read-host "Enter a password for the PFX file" -AsSecureString
            $pfxPassword = Decrypt-SecureString $securePfxPassword
        }
        $certFile = resolve-path $certFile
        $arguments = @("pkcs12", "-export", "-out", "`"$outfile`"", "-in", "`"$certfile`"", 
            "-name", "`"$displayname`"", "-passout", "`"pass:$pfxPassword`"")
        if ($keyFile) {
            $keyFile = resolve-path $keyFile
            $arguments += @("-inkey", "`"$keyFile`"")
        }
        Invoke-OpenSsl -argumentList $arguments
    }
    function Convert-OpenSSLPfxToPem {
        param(
            [parameter(mandatory=$true)] [string] $pfxfile,
            [string] $outfile = ((resolve-path $pfxfile).path -replace ".pfx$","") + ".pem"
        )
        Invoke-OpenSsl -argumentList @("pkcs12", "-in", "`"$pfxfile`"", "-out", "`"$outfile`"", "-nodes")
    }
    function Get-OpenSSLThumbprint {
        param(
            [parameter(mandatory=$true)] [string] $pemFile
        )
        $pemFile = resolve-path $pemFile
        $sslProc = Invoke-OpenSsl -Passthru -argumentList @("x509", "-in", "$pemFile", "-sha1", "-noout", "-fingerprint")
        $thumbprint = $sslProc.SerializedStandardOutput.Split('=')[1].Replace(':','')
        return $thumbprint
    }
    
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
    
    function Connect-VPN {
        $DomainCreds = Get-DLPProtectedCredential -credentialFile ~/credentials-MRLDLP.xml
        ipsecc -a -r FlameLeafVPN.vpn -u $DomainCreds.Username -p "$($DomainCreds.DecryptPassword())"
    }
    
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
}
