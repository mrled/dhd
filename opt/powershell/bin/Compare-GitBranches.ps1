<#
.synopsis
Compare each git branch to a (probably smaller) set of branches
.description
Use this script to make sense of a repository with many branches. See which feature branches are already merged and can be deleted, which branches are identical to one of your comparison branches, and which ones have diverged from your comparison branches.
.parameter RemoteName
The name of the git remote. Frequently "origin".
.parameter CompareBranches
A list of branches to use as comparison. Frequently you'll use some combination of "master","development","production"
.parameter RepositoryPath
cd here first. (Otherwise assumes $pwd)
.parameter FetchAllFirst
Do a 'git fetch --all' before comparing
#>
param(
    [parameter(mandatory=$true)] [string] $remoteName,
    [parameter(mandatory=$true)] [string[]] $compareBranches,
    [string] $repositoryPath = .,
    [switch] $fetchAllFirst
)

$significantCommitDigits = 7

function Get-SignificantCommitDigits() {
    param([string]$commitHash, [int]$sigDigits=7)
    return $commitHash.substring(0, $sigDigits)
}

function Get-GitBranches {
    param($remoteName)
    $gitBranches = git branch -r |
        ? { $_ -match "  $remoteName/" } |
        % { $_ -replace "  $remoteName/",'' -replace "" } |
        ? { $_ -notmatch "^HEAD" }
    return $gitBranches
}

function Get-GitCommitsInBranch {
    param([string] $branchName, [switch] $short)
    $fullLog = git log --oneline $branchName
    $commits = $fullLog |% { $_ -replace " .*","" }
    return $commits
}

function Get-GitHeadCommit {
    param([string] $branchName, [switch] $short)
    $fullLog = git log --oneline $branchName
    $lastCommit = $fullLog[0] -replace " .*",''
    if ($short) { $lastCommit = Get-SignificantCommitDigits $lastCommit }
    return $lastCommit
}

pushd $repositoryPath

if ($fetchAllFirst) { git fetch --all }

foreach ($branchA in $compareBranches) {
    $branchACommits = Get-GitCommitsInBranch -short -branch "$remoteName/$branchA"
    $branchAHead = $branchACommits[0]
    write-host "BRANCH A: '$branchA' (@$branchAHead) as comparison base"

    foreach ($branchB in (Get-GitBranches $remoteName)) {
        $branchBCommits = Get-GitCommitsInBranch -short -branch "$remoteName/$branchB"
        $branchBHead = $branchBCommits[0]
        $compareMessage = "  BRANCH B: '$branchB' (@$branchBHead)"

        $mergeBase = Get-SignificantCommitDigits -commitHash (git merge-base "$remoteName/$branchA" "$remotename/$branchB")

        if ($branchAHead -match $branchBHead) {
            write-host -foreground green "$compareMessage is identical to '$branchA'"
        }
        elseif ($branchBHead -match $mergeBase) {
            write-host -foreground green "$compareMessage is fully merged into '$branchA'"
        }
        else {
            write-host -foreground yellow "$compareMessage has diverged from '$branchA'"
        }
    }
}

popd
