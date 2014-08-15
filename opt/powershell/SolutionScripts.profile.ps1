$PromptSuffixOverride = {
    # Find something that looks sort of like the visual studio symbol...
    write-host " $([char]42479) " -nonewline -foregroundcolor White -backgroundcolor Magenta
}

$profile | Add-Member -MemberType NoteProperty -Name "SolutionScripts" -Value $myinvocation.mycommand.path -force
$repoRoot = resolve-path "C:\Projects\DLP\Ed-Fi-Ods"
cd $repoRoot
. $repoRoot\Initialize-PowershellForDevelopment.ps1

# Clear $error because fucking psget helpfully pre-populates it
$error.clear()