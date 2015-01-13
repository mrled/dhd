$DLPHelperDefaultProjectConfigFile = "$PSScriptRoot\DLPOrganizations.xml"
$DLPHelperProjectBase = "$HOME\Documents\DLPClients"

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
    set-alias GitExe "${env:ProgramFiles(x86)}\Git\bin\git.exe"
    $DlpOrganizations = Get-DLPProject
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

function Update-DLPProjectGitRepository {
    [CmdletBinding()]
    param(
        [string[]] $clientList,
        [switch] $WhatIf
    )

    $workingClientList = @()
    $DlpOrganizations = Get-DLPProject
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

    $serializedDlpOrganizations = Out-CliXml $DlpOrganizations -depth 4
    foreach ($client in $workingClientList) {
        start-job -name "git-$($client.LocalName)" -scriptBlock $clientUpdateSb -argumentList $client.LocalName,$WhatIf
    }
}

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

