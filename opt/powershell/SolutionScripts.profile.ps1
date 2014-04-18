$promptSuffix = {
    Write-Host "SS" -nonewline -foregroundcolor White
    Write-Host "ϫ" -nonewline -foregroundcolor Magenta
}

$profile | Add-Member -MemberType NoteProperty -Name "SolutionScripts" -Value $myinvocation.mycommand.path -force
$ssPath = get-item C:\Projects\DLP\TDOE-RestApiSpike\src\SolutionScripts
write-host "Solution Scripts Console" -foregroundcolor Magenta
cd $ssPath |out-null
foreach ($file in (gci $ssPath)) {
    if ($file.extension -eq '.ps1') {
        Write-Host "        Sourcing: $file" -foregroundcolor Magenta
        . $file.fullname
    }
    if ($file.extension -eq '.psm1')
    {
        Write-Host "Importing Module: $file" -foregroundcolor Magenta
        Import-Module $file.fullname -Force
    }
}
set-alias initdev Initialize-DevelopmentEnvironment
