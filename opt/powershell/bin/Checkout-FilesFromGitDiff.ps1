<#
.synopsis
Checkout files that a 'git diff' reports as having changed between the current checkout and a different branch
#>
[cmdletbinding()]
param(
    [parameter(mandatory=$true)] [string] $branch,
    [string] $path,
    [switch] $whatIf
)

$gitCmd = "git diff --name-only $branch"
if ($path) { $gitCmd += " -- $path" }
$changedFiles = invoke-expression $gitCmd

foreach ($file in $changedFiles) {
    if ($whatIf) { write-host "$file" }
    else { git checkout "$branch" -- "$file" }
}
