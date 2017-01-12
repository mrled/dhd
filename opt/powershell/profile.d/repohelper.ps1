# This is mostly just useful because Windows *still* fucking suxxx in 2017 and will throw a fit if you have long paths. Fine, you piece of shit, I'll use a fake drive. Christ.
$repoBase = "~/Documents/Repositories"
if (Test-Path $repoBase) {
    $repoBase = Resolve-Path $repoBase | Select-Object -ExpandProperty Path
    $repoDrive = "R"
    if (-not (Get-PsDrive | Where-Object -Property Name -EQ $repoDrive)) {
        subst.exe "${repoDrive}:" $repoBase
        Get-PsDrive | Out-Null # Apparently Powershell won't notice the new drive unless you do this
    }
}

<#
.synopsis
Add a managed Git working tree of your current repository, assuming it's managed by GitHub
.description
1)  You must be in an existing Git working directory
2)  Assume a layout of:
        <repo base dir>/
            mains            The main working tree / initial checkout location
                repo1
                repo2
                ...
            remote1
                branch1
                    repo1
                    repo2
                    ...
                branch2
                ...
            remote2
            ...
3) Running this function will create a new linked work tree in the working directory, organized by remote and branch name
.parameter remote
The name of a Git remote, such as "origin"
.parameter branch
The name of a branch on the Git remote you're using, such as "master"
.notes
See also: https://git-scm.com/docs/git-worktree
Note that when you're done with a linked working tree, you can just use your normal OS tools to delete it; Git will notice this and clean up after you on its own time. Neat.
.example
Add-ManagedGitWorkingTree -remote origin -branch master
#>
function Add-ManagedGitWorkingTree {
    [CmdletBinding()] Param(
        [Parameter(Position=0, Mandatory=$True)] $remote,
        [Parameter(Position=1, Mandatory=$True)] $branch
    )

    $localBranch = "$remote-$branch"

    # Make sure we're in a Git working directory
    git diff --quiet | Out-Null
    if ($LASTEXITCODE -eq 129) {
        throw "Not in a Git working directory"
    }

    # Get the name of the repository (assume it's the same locally and remotely)
    $repoName = Get-Item . | Select-Object -ExpandProperty Name

    # Create the branch if it doesn't exist. (`git worktree add -b BRANCHNAME` will create the branch if it doesn't exist, but will fail if it does exist, so we just create it here if we need it)
    try {
        git rev-parse --verify $localBranch 2>&1 | Out-Null
    }
    catch {
        git branch --track $localBranch $remote/$branch
    }

    # Create the final location of the linked worktree
    $linkedDir = "../../$remote/$branch/$repoName"
    mkdir -Force $linkedDir | Out-Null

    # Actually add the linked clone
    git worktree add $linkedDir $localBranch
}
