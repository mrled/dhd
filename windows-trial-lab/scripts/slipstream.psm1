import-module dism -verbose:$false

# TODO: Copy-ItemAndExclude
# function Copy-ItemAndExclude {
#     [cmdletbinding()] param(
#         [parameter(mandatory=$true)] [string] $path,
#         [parameter(mandatory=$true)] [string] $destination,
#         [parameter(mandatory=$true)] [string[]] $exclude,
#         [switch] $force
#     )
#     $path = resolve-path $path | select -expand path
#     $sourceItems = Get-ChildItem -Path $path -Recurse -Exclude $exclude 
#     Write-EventLogWrapper "Found $($sourceItems.count) items to copy from '$path'"
#     #$sourceItems | copy-item -force:$force -destination {Join-Path $destination $_.FullName.Substring($path.length)}
#     $sourceItems | copy-item -force:$force -destination {
#         if ($_.GetType() -eq [System.IO.FileInfo]) {
#             Join-Path $destination $_.FullName.Substring($path.length)
#         } 
#         else {
#             Join-Path $destination $_.Parent.FullName.Substring($path.length)
#         }
#     }
# }


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
    Write-EventLogWrapper "Found the WAIK at '$adkPath'"

    $arch = Get-OSArchitecture
    switch ($arch) {
        $ArchitectureId.i386 { 
            $formatted = $pathFormatString -f $adkPath,$waikArch
            if (test-path $formatted) { return $formatted }
        }
        $ArchitectureId.amd64 { 
            foreach ($waikArch in @("amd64","x64")) {
                $formatted = $pathFormatString -f $adkPath,$waikArch
                if (test-path $formatted) { return $formatted }
            }
        }
        default { 
            throw "Could not determine architecture of '$arch'" 
        }
    }
    throw "Could not resolve format string '$pathFormatString' to an existing path"
}


function New-WindowsInstallMedia { # TODO fixme not sure I wanna handle temp dirs this way??
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $sourceIsoPath,
        [parameter(mandatory=$true)] [string] $installMediaTemp,  # WILL BE DELETED
        [parameter(mandatory=$true)] [string] $installWimPath,    # your new install.wim file
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
    Write-EventLogWrapper "Calling OSCDIMG: '$oscdimgCall"
    Invoke-ExpressionAndCheck $oscdimgCall -verbose:$verbose

    dismount-diskimage -imagepath $sourceIsoPath
}


<#
.notes
The install.wim file doesn't (ever? sometimes?) denote architecture in its image names, but boot.wim (always? usually?) does
#>
function Get-BootWimArchitecture {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] $wimFile
    )
    $bootWimInfo = Get-WindowsImage -imagePath $wimFile -verbose:$verbose

    $arch = $null
    if (-not $bootWimInfo) { throw "Got no information for wimfile at '$wimFile'"}
    elseif ($bootWimInfo[0].ImageName -match "x86") { $arch = $ArchitectureId.i386 }
    elseif ($bootWimInfo[0].ImageName -match "x64") { $arch = $ArchitectureId.amd64 }
    else { throw "Could not determine architecture for '$wimFile'"}

    write-verbose "Found an architecture of '$arch' for '$wimFile'"
    return $arch
}


function Apply-WindowsUpdatesToIso {
    [cmdletbinding()] param (
        [parameter(mandatory=$true)] [string] $inputIso,
        [parameter(mandatory=$true)] [string] $outputIso,
        [parameter(mandatory=$true)] [string] $wsusOfflineDir,
        [parameter(mandatory=$true)] [string] $wimMountDir
    )

    $myWimMounts = @()

    mount-diskimage -imagepath $inputIso
    $mountedDrive = get-diskimage -imagepath $inputIso | get-volume | select -expand DriveLetter

    $installWim = "$labTempDir\install.wim"
    if (-not (test-path $installWim)) { 
        cp "${mountedDrive}:\Sources\install.wim" $labTempDir -verbose:$verbose
    }
    else {
        write-verbose "Using EXISTING install.wim at '$installWim'"
    }
    Set-ItemProperty -path $installWim -name IsReadOnly -value $false -force 

    $arch = Get-BootWimArchitecture -wimFile "${mountedDrive}:\sources\boot.wim" -verbose:$verbose
    dismount-diskimage -imagepath $inputIso

    $wimInfo = Get-WindowsImage -imagePath $installWim
    $shortCode = Get-WOShortCode -OSName $wimInfo[0].ImageName -OSArchitecture $arch
    #$updatePath = resolve-path "${wsusOfflineDir}\client\$shortCode\glb" | select -expand Path
	$updatePath = "D:\iso\wintriallab\temp-slipstream\WSUSCache\w63-i386-glb"

    foreach ($wimInfo in (Get-WindowsImage -imagePath $installWim)) {
        $wimMountSubdir = mkdir "${wimMountDir}\$($wimInfo.ImageIndex)" -force | select -expand fullname
        Mount-WindowsImage -imagePath $installWim -index $wimInfo.ImageIndex -path $wimMountSubdir

		write-verbose "Applying '$((ls $updatePath).count)' updates to '$wimInfo'''"
        try {
            Add-WindowsPackage -PackagePath $updatePath -path $wimMountSubdir
        }
        catch {
            write-verbose "Caught error(s) when installing packages:`n`n$_`n"
        }
        
        Dismount-WindowsImage -Path $wimMountSubdir -Save 
    }

    New-WindowsInstallMedia -sourceIsoPath $inputIso -installMediaTemp $installMediaTemp -installWimPath $installWim -outputIsoPath $outputIso
}