set-alias nuget "${env:ChocolateyInstall}\chocolateyinstall\nuget.exe"
$choco = $env:ChocolateyInstall

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

# Ugh you have to run this from an elevated prompt lmao
function Get-WebPiPackages {
    [cmdletbinding(DefaultParameterSetName="DefaultCache")]
    param(
        $search,
        $ignore,
        [ValidateSet("Installed","Application","Available")] $type,
        [parameter(ParameterSetName="DefaultCache")] [ValidateSet("Force","No")] $Refresh,
        [parameter(ParameterSetName="SpecifyCache")] $CacheFile
    )

    # Allow myself to use this fucking thing without taking 10 minutes to call webpi cli every fucking time
    # However, don't allow a specified cache to be updated
    if ($PSCmdlet.ParameterSetName -eq "DefaultCache") {
        $cacheFile = "${env:Temp}\mrledWebPiOutputPackageCache.txt"
        $yesterday = (get-date).AddDays(-1)
        <# Refresh the cache if one of the following conditions is met:
             - "-Refresh Force" is specified
             - "-Refresh" is not specified at all, but the cache file is more than one day old
             - The cache file doesn't exist
        #>
        if (($refresh -eq "Force") -or
            (-not (test-path $cacheFile)) -or
            ((-not $refresh) -and ($yesterday -gt (get-item $cacheFile).LastWriteTime)))
        {
            WebPiCmd /list /listoption:all | out-file $cacheFile -encoding utf8
        }
    }
    $cacheFile = resolve-path $cacheFile
    $rawWpiOutput = get-content $cacheFile
    $indexOfHeaderEnd = $rawWpiOutput.indexof('Current language of installers is English')

    $allWpiPackages = @()
    $packageType = $false
    $lineNum = 0
    foreach ($packageLine in $rawWpiOutput[($indexOfHeaderEnd+1)..($rawWpiOutput.count-2)]) {
        if ([string]::IsNullOrWhiteSpace($packageLine)) {}   # Ignore whitespace
        elseif ($packageLine -match '^-+$') {}               # Ignore header lines of just dashss
        elseif ($packageLine -match '^ID\s+Title$') {}       # Ignore header lines of column names
        # Output from the webpi cli util organizes the package list into 3 sections:
        elseif ($packageLine -match '--Previously Installed Products') {
            $packageType = "Installed"
        }
        elseif ($packageLine -match '--Applications') {
            $packageType = "Application"
        }
        elseif ($packageLine -match '--Available Products') {
            $packageType = "Available"
        }
        elseif (-not $packageType) {
            throw "Failed to process line ${cacheFile}:${lineNum} `n" +
                "    '$packageLine'"
        }
        else {
            $packageLine -match '(\S*)\s*(.*)' | out-null
            $properties = [ordered]@{ Name=$matches[1]; Type=$packageType; Description=$matches[2]; }
            $allWpiPackages += @( New-Object -type PSObject -property $properties )
        }
        $lineNum += 1
    }

    if ($type) {
        $typeMatches = $allWpiPackages |? { $_.type -match $type }
    }
    else {
        $typeMatches = $allWpiPackages
    }

    if ($search) {
        $searchMatches = @()
        foreach ($package in $typeMatches) {
            if (($package.name -match $search) -or ($package.Description -match $search)) {
                $searchMatches += @($package)
            }
        }
    }
    else {
        $searchMatches = $typeMatches
    }

    if ($ignore) {
        $ignoreMatches = @()
        foreach ($package in $searchMatches) {
            if (($package.name -notmatch $ignore) -and ($packge.Description -notmatch $ignore)) {
                $ignoreMatches += @($package)
            }
        }
    }
    else {
        $ignoreMatches = $searchMatches
    }

    return $searchMatches
}

function Get-ChocolateyPackages {
    param(
        [string[]] $packageName 
    )
    write-host -foreground cyan "Searching Chocolatey repos..."
    iex "choco list $($packageName -join " ")"

    write-host -foreground cyan "Searching WebPI repo..."
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
            write-host -foreground cyan "Searching NuGet repo: $($source.Name)"
            iex "choco list $($packageName -join ' ') -source `"$($source.Name)`"" 
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

function Get-InstalledChocolateyPackages {
    # This is actually just getting ones with a directory in $env:ChocolateyInstall\lib\
    # So not, for instance, webpi stuff.
    (gci $env:ChocolateyInstall\lib).name | %{ $_ -match "^(.+?)\.\d+\.\d.*$" | Out-Null; $matches[1]} | % { echo $_}
}