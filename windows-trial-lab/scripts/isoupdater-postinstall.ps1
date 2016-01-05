<#
This script is part of my ISO updater workflow. 

It expects to be run on a machine that has applied Windows updates, but not deleted its cache directory at `${env:WinDir}\SoftwareDistribution\Download`.

It downloads the Windows trial ISO that corresponds to the machine that runs it and applies all Windows Updates to that ISO that are downloaded to its cache directory. Note that only MSI updates can be applied this way. The vast majority of updates are MSI updates, but other types of updates such as EXE updates cannot be applied to an ISO.
#>

[cmdletbinding()] param(
    $WorkingDirectory
)

import-module $PSScriptRoot\wintriallab-postinstall.psm1
$errorActionPreference = "Stop"

<#
.description
Get the path of the Windows ADK or AIK or whatever the fuck they're calling it from a format string
- {0} is always the WAIK directory
    - e.g. "C:\Program Files (x86)\Windows Kits\8.1\"
    - e.g. "X:\Program Files\Windows Kits\8.0"
- {1} is always the host architecture (x86 or amd64)
    - i THINK this is right, but I don't understand WHY. why do you need an amd64 version of oscdimg.exe?
    - however, there are arm executables lying around, and i definitely can't execute those. wtf?

So we expect a string like "{0}\bin\{1}\wsutil.exe"
#>
function Get-AdkPath {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $pathFormatString
    )

    $adkPath = ""
    $possibleAdkPaths = @("${env:ProgramFiles(x86)}\Windows Kits\8.1","${env:ProgramFiles}\Windows Kits\8.1")
    $possibleAdkPaths |% { if (test-path $_) { $adkPath = $_ } }
    if (-not $adkPath) { throw "Could not find the Windows Automated Installation Kit" }
    write-verbose "Found the WAIK at '$adkPath'"

    $arch = Get-OSArchitecture
    switch ($arch) {
        $ArchitectureId.i386 {
            $formatted = $pathFormatString -f $adkPath,$arch
            if (test-path $formatted) { return $formatted }
        }
        $ArchitectureId.amd64 {
            foreach ($goddammit in @("amd64","x64")) {
                $formatted = $pathFormatString -f $adkPath,$goddammit
                if (test-path $formatted) { return $formatted }
            }
        }
        default {
            throw "Could not determine path for format string '$pathFormatString' for host architecture of '$arch'"
        }
    }
    throw "Could not resolve format string '$pathFormatString' to an existing path"
}

<#
.synopsis
Create a new Windows ISO from a working Windows ISO + a new install.wim file
.parameter SourceIsoPath
Path of a working ISO
.parameter InstallMediaTemp
A temporary directory. NOTE: THE CONTENTS OF THIS DIRECTORY WILL BE ERASED
.parameter InstallWimPath
A new install.wim to use
.parameter OutputIsoPath
Path of the resulting ISO
#>
function New-WindowsInstallMedia {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $sourceIsoPath,
        [parameter(mandatory=$true)] [string] $installMediaTemp,
        [parameter(mandatory=$true)] [string] $installWimPath,
        [parameter(mandatory=$true)] [string] $outputIsoPath
    )
    $oscdImgPath = Get-AdkPath "{0}\Assessment and Deployment Kit\Deployment Tools\{1}\Oscdimg\oscdimg.exe"
    $installWimPath = resolve-path $installWimPath | select -expand path
    $installMediaTemp = mkdir -force $installMediaTemp | select -expand fullname

    $outputIsoParentPath = split-path $outputIsoPath -parent
    $outputIsoFilename = split-path $outputIsoPath -leaf
    $outputIsoParentPath = mkdir -force $outputIsoParentPath | select -expand fullname

    if (test-path $installMediaTemp) { rm -recurse -force $installMediaTemp }
    mkdir -force $installMediaTemp | out-null

    $diskVol = get-diskimage -imagepath $sourceIsoPath | get-volume
    if (-not $diskVol) {
        mount-diskimage -imagepath $sourceIsoPath
        $diskVol = get-diskimage -imagepath $sourceIsoPath | get-volume
    }
    $driveLetter = $diskVol | select -expand DriveLetter
    $existingInstallMediaDir = "${driveLetter}:"

    # TODO: the first copy here copies the original install.wim, and the second copies the new one over it
    # this is really fucking dumb right? but then, THIS is way fucking dumber:
    # http://stackoverflow.com/questions/731752/exclude-list-in-powershell-copy-item-does-not-appear-to-be-working
    # PS none of those solutions are generic enough to get included so fuck it
    copy-item -recurse -path "$existingInstallMediaDir\*" -destination "$installMediaTemp" -verbose:$verbose
    remove-item -force -path "$installMediaTemp\sources\install.wim"
    copy-item -path $installWimPath -destination "$installMediaTemp\sources\install.wim" -force -verbose:$verbose

    $etfsBoot = resolve-path "$existingInstallMediaDir\boot\etfsboot.com" | select -expand Path
    $oscdimgCall = '& "{0}" -m -n -b"{1}" "{2}" "{3}"' -f @($oscdImgPath, $etfsBoot, $installMediaTemp, $outputIsoPath)
    write-verbose "Calling OSCDIMG: '$oscdimgCall"
    Invoke-ExpressionAndCheck $oscdimgCall -verbose:$verbose

    dismount-diskimage -imagepath $sourceIsoPath
}

<#
.synopsis
Download the Windows trial ISO that corresponds to the host OS, copy its install.wim, apply Windows Updates to the copy from the local WSUS cache, and create a new ISO containing the updated install.wim
.notes
TODO: could use some cleanup
TODO: improve documentation of parameters
#>
function Apply-WindowsUpdatesToTrialIso {
    [cmdletbinding(DefaultParameterSetName="CorrespondingVersion")] 
    param(
        $WorkingDirectory = (New-TemporaryDirectory | Select -Expand FullName),
        [Parameter(Mandatory=$True, ParameterSetName="CorrespondingVersion")] [switch] $CorrespondingVersion,
        [Parameter(Mandatory=$True, ParameterSetName="SpecifiedVersion")] $TrialIsoInfo,
        [Parameter(Mandatory=$True, ParameterSetName="SpecifiedVersion")] $Architecture,
        [Parameter(Mandatory=$True, ParameterSetName="SpecifiedVersion")] $WindowsUpdateCacheDir
    )

    if ($PsCmdlet.ParameterSetName -match "$CorrespondingVersion") {
        $TrialIsoInfo = Get-WindowsTrialISO
        $Architecture = Get-OSArchitecture
        $WindowsUpdateCacheDir = "${env:WinDir}\SoftwareDistribution\Download"
    }

    $WorkingDirectory = mkdir -Force $WorkingDirectory | Select -Expand FullName

    $hostWinVer = [Environment]::OSVersion.Version
    $filenameVersionStamp = "$($hostWinVer.Major)-$($hostWinVer.Minor)-$Architecture"
    $dateStamp = Get-Date -Format get-date -format yyyy-MM-dd
    $pristineIsoPath = "$WorkingDirectory\Windows-$filenameVersionStamp-Pristine.iso"
    $updatedIsoPath  = "$WorkingDirectory\Windows-$filenameVersionStamp-Updated-$dateStamp.iso"
    $installWimFilePath = "$WorkingDirectory\install.wim"
    $installWimMountPath = "$WorkingDirectory\mnt"
    $nwimTemp = "$WorkingDirectory\NewWindowsInstallMediaTemp"

    Get-WebUrl -url $TrialIsoInfo.URL -outFile $pristineIsoPath
    $mountedIso = Mount-DiskImage -ImagePath $pristineIsoPath -PassThru
    cp "$($mountedIso.DevicePath)\sources\install.wim" $installWimFilePath
    Set-ItemProperty -Path $installWimFilePath -Name IsReadOnly -Value $false -Force
    Dismount-DiskImage -ImagePath $pristineIsoPath

    foreach ($wimInfo in (Get-WindowsImage -ImagePath $installWimFilePath)) {
        $wimMountSubdir = mkdir "${installWimMountPath}\$(wimInfo.ImageIndex)" -force | Select -Expand FullName
        Mount-WindowsImage -ImagePath $installWimFilePath -Index $wimInfo.ImageIndex -Path $wimMountSubdir
        try {
            Add-WindowsPackage -PackagePath $WindowsUpdateCacheDir -Path $wimMountSubdir
        }
        catch {
            write-verbose "Caught error(s) when installing packages:`n`n$_`n"
        }
        Dismount-WindowsImage -Path $wimMountSubdir -Save
    }

    New-WindowsInstallMedia -SourceIsoPath $pristineIsoPath -InstallMediaTemp $nwimTemp -InstallWimPath $installWimFilePath -OutputIsoPath $updatedIsoPath
}

throw "Need to install oscdimg.exe and the DISM Powershell module somehow...?"
Apply-WindowsUpdatesToTrialIso -CorrespondingVersion
