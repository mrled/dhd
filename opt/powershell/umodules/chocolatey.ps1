#set-alias choco C:\Chocolatey\chocolateyinstall\chocolatey.cmd
set-alias nuget C:\Chocolatey\chocolateyinstall\nuget.exe

function Get-NuGetSources {
    $nugetSources = @()
    $sourceString = nuget sources
    $sourceString = $sourceString[2..$sourceString.length]
    foreach ($ln in (0..($sourceString.count-1))) {
        if (($ln %2) -eq 0) {
            $nameLine = $sourceString[$ln]
            $sourceLine = $sourceString[$ln+1]
            $nugetSources += @(@{
                Name = $nameLine[6..($nameLine.IndexOf(" [")-1)] -join ''
                Location = $sourceLine -replace ' ',''
                Type = "NuGet"
                Enabled = $nameLine.Contains("[Enabled]")
            })
        }
    }
    return $nugetSources
}

function Get-ChocolateySources {

    $allSources = @()

    $allSources += @({
        Name = "WebPI"
        Location = "WebPI"
        Type = "WebPI"
        Enabled = $true
    })

    $chocoSources = choco sources
    $chocoSources = $chocoSources[3..($chocoSources.count-4)] # strip extra newlines and bullshit
    foreach ($line in $chocoSources) {
        $name,$location,$null = $line -split " +"
        $allSources += @(@{
            Name = $name
            Location = $location
            Type = "Chocolatey"
            Enabled = $true
        })
    }

    $allSources += @(Get-NuGetSources)

    $allSources | ft
}

function Get-WebPiPackages {
    $webpiOutput = choco list -source WebPI |? { -not [string]::IsNullOrWhiteSpace($_) }
    $indexOfDivider = $webpiOutput.indexof('----------------------------------------') 
    $webpiPackages = $webpiOutput[($indexOfDivider+1)..($webpiOutput.count-2)]
}

function Get-ChocolateyPackages {
    param(
        [string[]] $packageName 
    )
    write-host "Searching Chocolatey repos..."
    iex "choco list $($packageName -join " ")"

    write-host "Searching WebPI repo..."
    $webpiPackages = Get-WebPiPackages
    foreach ($wpp in $webpiPackages) {
        foreach ($pn in $packageName) {
            if ($wpp -like "*$pn$") {
                write-host $wpp
            }
        }
    }

    foreach ($source in Get-NuGetSources) {
        if ($source.Enabled) {
            write-host "Searching NuGet repo: $($source.Name)"
            iex "choco list $($packageName -join " ") -source `"$($source.Name)`"" 
<#
            |? { 
                (-not $_.contains('Reading environment variables from registry.')) -and
                (-not $_.contains('No packages found.'))
            }
#>
<#
            $name, $version = $output -split ' '
            $allPackages += @(@{
                Name = $name
                Version = $version
                Source = $source.Name
                Type = $source.Type
                Location = $source.Location
            })
#>
        }
    }
}
