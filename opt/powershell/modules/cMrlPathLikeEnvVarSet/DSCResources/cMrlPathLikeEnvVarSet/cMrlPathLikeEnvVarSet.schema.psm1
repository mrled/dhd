<#
.SYNOPSIS
A DSC resource module template
#>

Configuration cMrlPathLikeEnvVarSet {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $Name,
        [Parameter(Mandatory)] [string[]] $Location,
        [string] $Ensure = "Present",
        [string] $InsertionMode = "Append",
        [bool] $OnlyIfExists
    )

    $ErrorActionPreference = "Stop"

    Import-DscResource -ModuleName cMrlPathLikeEnvVar

    $idx = 0
    foreach ($loc in $Location) {
        $target = if ($PsDscRunAsCredential) {$PsDscRunAsCredential.UserName} else {"Machine"}
        cMrlPathLikeEnvVar "cMrlPathLikeEnvVar_${Ensure}_${target}_${Name}_${idx}_${loc}" {
            Name = $Name
            Location = $loc
            Ensure = $Ensure
            InsertionMode = $InsertionMode
            OnlyIfExists = $OnlyIfExists
        }
        $idx += 1
    }
}
