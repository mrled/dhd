$DLPHelperDefaultProjectConfigFile = "$PSScriptRoot\DLPOrganizations.xml"
#$DLPHelperProjectBase = "$HOME\Documents\DLPClients"
$DLPHelperProjectBase = "P:\"

function New-DLPProject {
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
        $projDir = "$DLPHelperProjectBase\$($this.Localname)"
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
    <# This turned out not to be necessary for something I was doing, but it's an interesting idea:
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

function Invoke-Git {
    [CmdletBinding()]
    param(
        [switch] $WhatIf,
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args
    )
    $GitExePath = "${env:ProgramFiles(x86)}\Git\bin\git.exe"
    set-alias git $GitExePath

    write-verbose "$GitExePath $args"
    if (-not $WhatIf) {
        git @args
        if ($LASTEXITCODE) {
            write-error "Git error while trying to run '$GitExePath $args'"
        }
    }
}

$clientUpdateSb = {
    # Note: job script blocks don't work very well with custom objects
    # I was having a problem where some members were present, but others, such as ScriptProperty members,
    # were not present when the object was passed into the script block
    # Job SBs also cannot use externally defined functions such as New-DLPProject or In-CliXml
    param(
        [parameter(mandatory=$true)] $clientName,
        [bool] $WhatIf = $false
    )

    import-module DLPHelper
    $DLPOrganizations = Get-DLPProject
    $clientOrganization = Get-DLPProject $clientName
    $localName = $clientOrganization.localName
    $GitHubOrg = $clientOrganization.GitHubOrg
    $projectDir = $clientOrganization.Directory

    write-output "==== Updating client '$clientName'... ===="
    if (-not $clientOrganization.Checkout) {
        write-output "Client '$LocalName' was not set to be checked out."
    }
    else {
        if (-not (test-path $projectDir)) {
            mkdir $projectDir | out-null
        }
        foreach ($repoName in $clientOrganization.Repositories) {
            push-location $projectDir 
            try {
                # $repoClients is all the organizations that contain $repoName
                $repoClients = ($DLPOrganizations |? { $_.Repositories -contains $repoName }).LocalName
                write-output "  All clients who fork this repo: $repoClients"
                if (-not (test-path "$projectDir\$repoName")) {
                    write-output "  Doing initial clone for '$repoName' for project '$localName'"
                    Invoke-Git clone --no-checkout --recursive --origin "$localName" "git@github.com:$GitHubOrg/$repoName" -whatif:$whatif
                    cd "$projectDir\$repoName"
                    $HEAD = (Invoke-Git branch -r | select-string "$localName/HEAD") -replace "  $localName/HEAD -> $localName/"
                    Invoke-Git checkout -b "${localName}-${HEAD}" "${localName}/${HEAD}"
                }
                else {
                    write-output "  Updating repository '$repoName' for project '$localName'"
                }

                set-location $projectDir\$repoName
                $gitRemotes = Invoke-Git remote
                foreach ($gitRemoteName in $repoClients) {
                    if ($gitRemotes -notcontains $gitRemoteName) {
                        write-output "    Adding '$gitRemoteName' remote for '$repoName' repository"
                        $ghoName = ($DLPOrganizations |? { $_.LocalName -match "^$gitRemoteName$" }).GitHubOrg
                        Invoke-Git remote add $gitRemoteName "git@github.com:$ghoName/$repoName" -whatif:$whatif
                    }
                    else {
                        write-output "    Found '$gitRemoteName' remote for '$repoName' repository"
                    }
                }
                write-output "  Doing 'git fetch' in repository '$repoName' for project '$localName'"
                Invoke-Git fetch --all -whatif:$whatif
            }
            finally {
                pop-location
            }
        }
        write-output "==== Finished with client '$clientName'... ===="
    }
}

function Update-DLPProjectGitRepository {
    [CmdletBinding()]
    param(
        [string[]] $clientList,
        [switch] $useJobs,
        [switch] $WhatIf
    )

    $workingClientList = @()
    $DLPOrganizations = Get-DLPProject
    foreach ($clientName in $clientList) {
        $client = $DLPOrganizations |? { $_.LocalName -eq $clientName }
        if (-not $client) {
            throw "Passed client name '$clientName', but no such client exists in `$DLPOrganizations"
        }
        write-verbose "Found client $($client.LocalName)"
        $workingClientList += @($client)
    }
    if (-not $clientList) {
        $workingClientList = $DLPOrganizations
    }
    write-verbose "Updating repositories for projects:"
    foreach ($c in $workingClientList) {
        write-verbose "    $($c.LocalName)"
    }

    $serializedDLPOrganizations = Out-CliXml $DLPOrganizations -depth 4
    foreach ($client in $workingClientList) {
        if ($useJobs) {
            start-job -name "git-$($client.LocalName)" -scriptBlock $clientUpdateSb -argumentList $client.LocalName,$WhatIf.IsPresent
        }
        else {
            invoke-command -scriptblock $clientUpdateSb -argumentList $client.LocalName,$WhatIf.IsPresent
        }
    }
}

<#
.description
Get the DLPProject object for a given project/client name
#>
function Get-DLPProject {
    [CmdletBinding()]
    param(
        [string[]] $projectName,
        [string] $configPath = $DLPHelperDefaultProjectConfigFile
    )
    $configPath = resolve-path $configPath
    $configXml = [xml](get-content $configPath)

    $workingProjects = @()
    foreach ($pn in $projectName) {
        $projElement = $configXml.projects.project |? { $_.localname -eq $pn }
        if (-not $projElement) {
            throw "Passed client name '$pn', but no such client exists in $configPath"
        }
        $workingProjects += @($projElement)
    }
    if (-not $projectName) {
        $workingProjects = $configXml.projects.project
    }

    $returnProjects = @()
    foreach ($proj in $workingProjects) {
        $ctx = $null
        if ($proj.contexts.context.repositories.repository.name) {
            $ctx = $proj.contexts.context |% -begin {$c=@{}} -process {$c[$_.name]=$_.repositories.repository.name} -end {$c}
        }
        $repositoryNames = @()
        foreach ($repo in $proj.repositories.repository) {
            if ($repo.forkof) {
                $repositoryNames
            }
        }
        $npparams = @{
            GitHubOrg = $proj.githubname
            LocalName = $proj.localname
            Checkout = [bool]$proj.Checkout
            Repositories = $proj.repositories.repository.name
            Contexts = $ctx
        }
        $returnProjects += @(New-DLPProject @npparams)

    }
    return $returnProjects
}

$DLPProjectFileTypes = @{}
$DLPProjectFileTypes.logistics = @{}
$DLPProjectFileTypes.logistics.include = @('*.ps1','*.psm1','*.psd1','credentials-*.xml')
$DLPProjectFileTypes.logistics.subdir = @('logistics','Application\SolutionScripts')
$DLPProjectFileTypes.source = @{}
$DLPProjectFileTypes.source.include = @('*.cs')
$DLPProjectFileTypes.source.subdir = @('Application')
$DLPProjectFileTypes.visualstudio = @{}
$DLPProjectFileTypes.visualstudio.include = @('*.sln','*.csproj','*.ccproj','*.config','*.cscfg','*.cspkg','*.csdef')
$DLPProjectFileTypes.visualstudio.subdir = @('Application')
$DLPProjectFileTypes.config = @{}
$DLPProjectFileTypes.config.include = @('*.config')
$DLPProjectFileTypes.config.exclude = @('packages.config','nuget.config','repositories.config')
$DLPProjectFileTypes.config.subdir = @('Application')
$DLPProjectFileTypes.database = @{}
$DLPProjectFileTypes.database.include = @('*.sql')
$DLPProjectFileTypes.database.subdir = @('Database')
$DLPProjectFileTypes.etl = @{}
$DLPProjectFileTypes.etl.include = @('*.dtsx')
$DLPProjectFileTypes.etl.subdir = @('Etl')
$DLPProjectFileTypes.alltypes = @{}
$DLPProjectFileTypes.alltypes.include = foreach ($k in $DLPProjectFileTypes.keys) { $DLPProjectFileTypes[$k]['include'] } 
$DLPProjectFileTypes.alltypes.exclude = foreach ($k in $DLPProjectFileTypes.keys) { $DLPProjectFileTypes[$k]['exclude'] } 
$DLPProjectFileTypes.alltypes.subdir = foreach ($k in $DLPProjectFileTypes.keys) { $DLPProjectFileTypes[$k]['subdir'] }
$DLPProjectFileTypes.allfiles = @{}
$DLPProjectFileTypes.allfiles.include = ''
$DLPProjectFileTypes.allfiles.subdir = '.'

function Get-DLPProjectFileTypes {
    return $DLPProjectFileTypes.Keys
}

<#
.synopsis
Find a DLP project file
.parameter Include
An array of patterns that matches the filenames you want to include, for example @('*.xml','*.html')
.parameter Exclude
An array of patterns that matches the filenames you want to exclude, for example @('*-credentials.xml')
.parameter Subdir
An array of subdirectories to search, for example @('Application')
.parameter Type
Rather than specifying -Include, -Exclude, and -Subdir together, specify a type. 
See Get-DLPProjectFileTypes to for a listing of available types
You can override a Type's default include/exclude/subdir by specifying that option in addition:
    -Type logistics -Include *.vars.ps1
That invocation will search in the logistics subdirs, but will replace the default includes with a much smaller list
.parameter ContainingPattern
Only return files that contain the pattern
.parameter CaseSensitive
Force the pattern to be case sensitive. (Ignored if -ContainingPattern is not passed.)
.parameter ProjectName
Search only a particular project
#>
function Get-DLPProjectFile {
    [CmdletBinding(DefaultParametersetName='CurrentProject')]
    param(
        [Parameter(Position=0)] [alias("query")] [string[]] $include,
        [string[]] $exclude,
        [string[]] $subdir,

        [ValidateScript({  (Get-DLPProjectFileTypes) -contains "$_"  })]
        [string[]] $type = 'logistics',

        [string]   $containingPattern,
        [switch]   $caseSensitive,

        [Parameter(ParameterSetName='ByProject')] [string[]] [alias('client')] $projectName,

        [Parameter(ParameterSetName='CurrentProject')] [switch] $inCurrentProject
    )

    if ($pscmdlet.ParameterSetName -eq "CurrentProject") {
        if (-not $pwd.path.startswith($DLPHelperProjectBase)) {
            throw "CWD is not a DLP project"
        }
        $splitSubPwd = $pwd.path.substring($DLPHelperProjectBase.length) -split '\\'
        if ($splitSubPwd[0]) {
            $projectName = $splitSubPwd[0]
        }
        else {
            $projectName = $splitSubPwd[1]
        }
    }   
    $projects = Get-DLPProject $projectName
    write-verbose "Found project(s): $($projects.LocalName)"

    if ($type -and -not $include) {
        $include = $DLPProjectFileTypes["$type"]['include']
    }
    if ($type -and -not $subdir) {
        $subdir = $DLPProjectFileTypes["$type"]['subdir'] 
    }
    if ($type -and -not $exclude) {
        $exclude = $DLPProjectFileTypes["$type"]['exclude'] 
    }

    $expandedLocations = @()
    foreach ($sd in @($subdir)) {
        foreach ($proj in @($projects)) {
            if (-not $proj.checkout) { 
                continue 
            }
            $projDir = $proj.Directory
            foreach ($repo in @($proj.repositories)) {
                $subdirFullPath = "$projDir\$repo\$sd"
                if (test-path $subdirFullPath) {
                    $expandedLocations += @($subdirFullPath)
                }
            }
        }
    }

    write-verbose "Looking in '$expandedLocations'"
    write-verbose "Trying to find files like '$include'"
    if ($exclude) { write-verbose "but ignoring files like '$exclude'" }

    $allMatches = gci -recurse $expandedLocations -include $include -exclude $exclude
    write-verbose "Found $($allMatches.count) total files"

    $noBins = $allMatches |? { $_.fullname -notmatch "\\bin\\" }

    if ($containingPattern) {
        $slsParms = @{
            quiet = $true
            pattern = $containingPattern
            CaseSensitive = $caseSensitive.ispresent
        }
        write-verbose "Searching through results files for strings matching '$containingPattern'..."
        $results = $noBins |? { $_ | sls @slsparms }
    }
    else {
        $results = $noBins
    }
    return $results
}
set-alias gdlp Get-DlpProjectFile

function Get-ClientConfigTransforms {
    [cmdletbinding(DefaultParametersetName="env")] param(
        [parameter(mandatory=$true,position=0)] [string] $client,
        [string] [ValidateSet("ODS","Dash","All")] $product = "All",
        [parameter(ParameterSetName="env")] [string] $environmentName,
        [parameter(ParameterSetName="allenvs")] [switch] $all,
        [parameter(ParameterSetName="proj")] [switch] $project
    )

    $clientPath = resolve-path "$dlpProjectDrive\$client"

    $fuckingODSProjects = @(
        "EdFi.Ods.Admin.Web"
        "EdFi.Ods.BulkLoad.Console"
        "EdFi.Ods.SwaggerUI"
        "EdFi.Ods.WebApi"
        "EdFi.Ods.BulkLoad.Services.Windows.BulkWorker"
        "EdFi.Ods.BulkLoad.Services.Windows.UploadWorker"
    )
    $fuckingDashProjects = @(
        "EdFi.Dashboards.Presentation.Web"
        "EdFi.Dashboards.SecurityTokenService.Web"
    )

    $projFolders = @()
    if (($product -eq "ODS") -or ($product -eq "All")) {
        $fuckingODSProjects |% { $projFolders += @(resolve-path $clientPath\Ed-Fi-ODS-Implementation\Application\$_) }
    }
    if (($product -eq "Dash") -or ($product -eq "All")) {
        $fuckingDashProjects |% { $projFolders += @(resolve-path $clientPath\Ed-Fi-Apps\Application\$_) }
    }

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

    foreach ($proj in $projFolders) {
        $paths = @("$proj\*")
        if (test-path $proj\AzureStartup) {
            $paths += @("$proj\AzureStartup\*")
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

    $oldVars = ls $buildActivity -filter "$fromEnvironment.vars.ps1"
    $newVars = $oldVars.fullname -replace "$fromEnvironment","$environmentName"
    $newFiles += @(copy-item $oldVars.fullname $newVars -passthru)

    $newFiles += @(new-item -ItemType file -Path "$buildActivity\credentials-$environmentName.xml" -force:$force)

    return $newFiles
}

function Get-OctopusVariables { 
    [cmdletbinding()] param(
        [parameter(mandatory=$true,position=0)] [string] $client,
        [string] [ValidateSet("ODS","Dash","All")] $product = "All",
        [string] $variableName
    )
    $retval = @()

    $configFiles = Get-ClientConfigTransforms -all -client $client -product:$product

    foreach ($configFile in $configFiles) {
        if ($variableName) {
            if (sls -inputObject $configFile -pattern $variableName) { 
                $retval += @($configFile)
            }
        }
        else {
            $matches = sls -inputObject $configFile -pattern "\#\{(\w*)\}" -AllMatches
            foreach ($match in $matches.matches) {
                $retval += @($match.groups[1].value)
            }
        }
    }
    $retval | sort -Unique
}

function Get-LogisticsFile {
    [cmdletbinding()] param(
        [string[]] $client = "*",
        [string[]] $repository = "*",
        [string[]] $namedPattern,
        [string[]] $notNamedPattern,
        [string] $containingPattern,
        [string] $notContainingPattern
    )

    $gciParams = @{}
    $gciParams["Recurse"] = $true
    $gciParams["File"] = $true

    $clientPath = Get-Item "${DLPHelperProjectBase}*" -include $client
    $repoPath = $clientPath |% { Get-Item "$_\*" -include $repository }
    $logisticsPath = $repoPath |? { Test-Path "$_\logistics" } |% { "$_\logistics" }
    $gciParams["Path"] = $logisticsPath

    if ($namedPattern) { $gciParams["Include"] = $namedPattern }
    if ($notNamedPattern) { $gciParams["Exclude"] = $notNamedPattern }

    Write-Verbose "GCI params: "
    $gciParams.keys |% { Write-Verbose "  $_ = $($gciParams[$_])"}
    #$logisticsFiles = Get-ChildItem @gciParams |? { $_.GetType().Name -eq "FileInfo" } 
    $logisticsFiles = Get-ChildItem @gciParams

    if ($containingPattern) {
        $logisticsFiles = $logisticsFiles |? { Select-String -Pattern $containingPattern -Path $_ -Quiet }
    }
    if ($notContainingPattern) {
        $logisticsFiles = $logisticsFiles |? { Select-String -NotMatch -Pattern $notContainingPattern -Path $_ -Quiet }
    }

    return $logisticsFiles
}