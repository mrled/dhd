# This is mostly just useful because Windows *still* fucking suxxx in 2017 and will throw a fit if you have long paths. Fine, you piece of shit, I'll use a fake drive. Christ.
$repoBase = "~/Documents/Repositories"
if (((Get-PowershellPlatform) -match "Win32NT") -and (Test-Path -Path $repoBase)) {
    $repoBase = Resolve-Path $repoBase | Select-Object -ExpandProperty Path
    $repoDrive = "R"
    if (-not (Get-PsDrive | Where-Object -Property Name -EQ $repoDrive)) {
        subst.exe "${repoDrive}:" $repoBase
        Get-PsDrive | Out-Null # Apparently Powershell won't notice the new drive unless you do this
    }
}
