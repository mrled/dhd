$PromptSuffixOverride = {
    #Write-Host "SS" -nonewline -foregroundcolor White
    #Write-host "$([char]1003)" -nonewline -foregroundcolor Magenta
    # Find something that looks sort of like the visual studio symbol...
    # [char]42479   ꗯ (VAI SYLLABLE GBE)
    # [char]1003    ϫ (COPTIC SMALL LETTER GANGIA)
    write-host " $([char]42479) " -nonewline -foregroundcolor White -backgroundcolor Magenta
}

$profile | Add-Member -MemberType NoteProperty -Name "SolutionScripts" -Value $myinvocation.mycommand.path -force
$repoRoot = resolve-path C:\Projects\DLP\TDOE-RestApiSpike
. $repoRoot\Initialize-PowershellForDevelopment.ps1
cd $repoRoot

