$PromptSuffixOverride = {
    # Find something that looks sort of like the visual studio symbol...
    write-host " $([char]42479) " -nonewline -foregroundcolor White -backgroundcolor Magenta
}

$profile | Add-Member -MemberType NoteProperty -Name "SolutionScripts" -Value $myinvocation.mycommand.path -force
$repoRoot = resolve-path C:\Projects\DLP\TDOE-RestApiSpike
. $repoRoot\Initialize-PowershellForDevelopment.ps1
cd $repoRoot

