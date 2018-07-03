<#
.SYNOPSIS
Load the required test artifacts, then dot-source the test script
#>
[CmdletBinding()] Param(
)

Invoke-Command -ScriptBlock {
    $qgLib = "$PSScriptRoot\Packages\YC.QuickGraph.3.7.3\lib\net45"
    if (-not (Test-Path -Path $qgLib)) {
        $spParams = @{
            FilePath = "NuGet.exe"
            ArgumentList = @("restore", "-PackagesDirectory", "packages")
            WorkingDirectory = $PSScriptRoot
            NoNewWindow = $true
            Wait = $true
        }
        Start-Process @spParams
    }
    Add-Type -Path @(
        "$qgLib\YC.QuickGraph.dll"
        "$qgLib\YC.QuickGraph.Data.dll"
        "$qgLib\YC.QuickGraph.Glee.dll"
        "$qgLib\YC.QuickGraph.Graphviz.dll"
        "$qgLib\YC.QuickGraph.Petri.dll"
    )

    Import-Module $PSScriptRoot
}

. $PSScriptRoot\test.ps1
