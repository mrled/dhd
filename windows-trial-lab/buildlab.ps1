<#
.synopsis 
Windows Trial lab management script
.parameter baseConfigName
The name of one of the subdirs like "windows_81_x86"
.parameter action
Which build actions do you want to perform? 
.parameter tag
A tag for the temporary directory, the output directory, and the resulting Vagrant box
#>
function buildlab {
[cmdletbinding()]
param(
    [parameter(mandatory=$true,ParameterSetName="BuildPacker")] 
    [parameter(mandatory=$true,ParameterSetName="AddToVagrant")] 
    [parameter(mandatory=$true,ParameterSetName="VagrantUp")]
    [string] $baseConfigName,

    [parameter(mandatory=$true,ParameterSetName="DownloadWSUS")] [switch] $DownloadWSUS,
    [parameter(mandatory=$true,ParameterSetName="ApplyWSUS")]    [switch] $ApplyWSUS,
    [parameter(mandatory=$true,ParameterSetName="BuildPacker")]  [switch] $BuildPacker,
    [parameter(mandatory=$true,ParameterSetName="AddToVagrant")] [switch] $AddToVagrant,
    [parameter(mandatory=$true,ParameterSetName="VagrantUp")]    [switch] $VagrantUp,

    [parameter(mandatory=$true,ParameterSetName="ApplyWSUS")] [string] $isoPath,

    [string] $baseOutDir = "D:\iso\wintriallab",
    [string] $tempDirOverride,
    [string] $tag,
    [switch] $force,
    [switch] $whatIf
)

import-module dism -verbose:$false

# Module useful for Download-URL at least. TODO: this mixes concerns and may not be ideal?
get-module wintriallab-postinstall | remove-module 
import-module $PSScriptRoot\scripts\postinstall\wintriallab-postinstall.psm1 -verbose:$false

Set-StrictMode -Version 2.0

# This seems to be required with strict mode? 
$verbose = $false
# This correctly covers -verbose -verbose:$false and -verbose:$true
if ($PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent -eq $true) {
    $verbose = $true
}

$dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
$packerOutDir = "$baseOutDir\PackerOut"
$packerCacheDir = "$baseOutDir\packer_cache"
$wsusOfflineDir = "$baseOutDir\wsusoffline"

$labTempDir = "$baseOutDir\temp-$dateStamp"
if ($tempDirOverride) { $labTempDir = $tempDirOverride }

$wimMountDir = "${labTempDir}\MountInstallWim"
$installMediaTemp = "${labTempDir}\InstallMedia"
$newMediaIsoPath = "${labTempDir}\windows.iso"

$errorActionPreference = "Stop"
$fullConfigName = "wintriallab-${baseConfigName}" 

set-alias packer (gcm packer | select -expand path)
set-alias vagrant (gcm vagrant | select -expand path)


$outDir = "${packerOutDir}\${fullConfigName}"
if ($tag) { $outDir += "-${tag}"}

$packerConfigRoot = "${PSScriptRoot}\${baseConfigName}"
$packerFile = "${packerConfigRoot}\${baseConfigName}.packerfile.json"
$packedBoxPath = "${outDir}\${baseConfigName}_virtualbox.box"
$vagrantTemplate = "${packerConfigRoot}\vagrantfile-${baseConfigName}.template"

function Download-WSUSOfflineUpdater {
    if (test-path $wsusOfflineDir) { 
        throw "WSUSOffline is already extracted to '$wsusOfflineDir'"
    }
    $filename = "wsusoffline101.zip"
    $url = "http://download.wsusoffline.net/$filename"
    $dlPath = "$labTempDir\$filename" 
    Get-WebUrl -url $url -downloadPath $dlPath
    $exDir = resolve-path "$wsusOfflineDir\.." # why the ".." ? because the zipfile puts everything in a 'wsusoffline' folder
    sevenzip x "$dlPath" "-o$exDir"
}
function Download-WindowsUpdates {
    set-alias DownloadUpdates "$wsusOfflineDir\cmd\DownloadUpdates.cmd"
    foreach ($product in @('w63','w63-x64','w100','w100-x64')) {
        DownloadUpdates $product glb /includedotnet /verify 
    }
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
    Set-ItemProperty -path $wimFile -name IsReadOnly -value $false -force 

    $arch = Get-BootWimArchitecture -wimFile "${mountedDrive}:\sources\boot.wim" -verbose:$verbose
    dismount-diskimage -imagepath $inputIso

    $wimInfo = Get-WindowsImage -imagePath $installWim
    $shortCode = Get-WOShortCode -OSName $wimInfo[0].ImageName -OSArchitecture $arch
    $updatePath = resolve-path "${wsusOfflineDir}\client\$shortCode\glb" | select -expand Path

    foreach ($wimInfo in (Get-WindowsImage -imagePath $installWim)) {
        write-verbose "Attempging to apply WSUS Offline Updates to $wimInfo)"
        $wimMountSubdir = mkdir "${wimMountDir}\$($wimInfo.ImageIndex)" -force | select -expand fullname
        Mount-WindowsImage -imagePath $installWim -index $wimInfo.ImageIndex -path $wimMountSubdir

        try {
            Add-WindowsPackage -PackagePath $updatePath -path $wimMountSubdir
        }
        catch {
            write-verbose "Caught error(s) when installing packages:`n`n$_`n"
        }
        # foreach ($update in (ls $updatePath\* -include *.cab,*.msu)) {
        #     write-verbose "Applying update at $update to directory at $wimMountSubdir"
        #     try {
        #         Add-WindowsPackage -PackagePath $update -path $wimMountSubdir | out-null
        #     }
        #     catch {
        #         write-verbose "Failed to add package '$update' to mounted WIM at '$wimMountSubdir' with error '$_'; continuing..."
        #     }
        # }
        
        Dismount-WindowsImage -Path $wimMountSubdir -Save 
    }

    New-WindowsInstallMedia -sourceIsoPath $inputIso -installMediaTemp $installMediaTemp -installWimPath $installWim -outputIsoPath $outputIso
}

function Build-PackerFile {
    [cmdletbinding()]
    param(
        [parameter(mandatory=$true)] $packerFile,
        [parameter(mandatory=$true)] $vagrantTemplate,
        [parameter(mandatory=$true)] [string] $vagrantBoxName,
        $tag,
        $packerCacheDir,
        $outDir,
        [switch] $force,
        [switch] $whatIf
    )

    $packerFile = get-item $packerFile
    write-host $packerFile
    if ($packerCacheDir) { $env:PACKER_CACHE_DIR = $packerCacheDir }

    if (test-path $outDir) {
        if ($force) { rm -force -recurse $outDir }
        else        { throw "Outdir already exists at '$outDir'" }
    }

    pushd (get-item $packerFile | select -expand fullname | split-path -parent)
    try { 
        write-host "Building packer file '$($packerFile.fullname)' to directory '$outDir'..."
        if (-not $whatif) {
            packer build -var "output_directory=$outDir" "$($packerFile.fullname)"
            if ($LASTEXITCODE -ne 0) { throw "External command failed with code $LASTEXITCODE" }
        }
    }
    finally {
        popd
    }
    $outBox = get-item $outDir\*.box
    if ($outBox.count -gt 1) { 
        throw "Somehow you came up with more than one box here: '$outBox'"
    }
    if ($outBox -notmatch [Regex]::Escape($packedBoxPath)) { 
        throw "Found an output box '$outBox', but it doesn't match the expected packed box path of '$packedBoxPath'"
    }
    cp "$vagrantTemplate" "$outDir\Vagrantfile"
    write-verbose "Packed .box file: '$packedBoxPath'"
}

function Add-BoxToVagrant {
    [cmdletbinding()]
    param(
        [parameter(mandatory=$true)] $vagrantBoxName,
        [parameter(mandatory=$true)] $packedBoxPath,
        [switch] $force,
        [switch] $whatIf
    )
    if (-not $whatIf) {
        $forceOption = ""
        if ($force) { $forceOption = "--force" }
        vagrant box add $forceOption --name $vagrantBoxName $packedBoxPath    
        if ($LASTEXITCODE -ne 0) { throw "External command failed with code '$LASTEXITCODE'" }
    }
}

function Run-VagrantBox { 
    [cmdletbinding()]
    param(
        [parameter(mandatory=$true)] $vagrantBoxName,
        [parameter(mandatory=$true)] $workingDirectory, # with a Vagrantfile in it
        [switch] $whatIf
    )
    if (-not $whatIf) {
        try {
            pushd $workingDirectory
            vagrant up
            if ($LASTEXITCODE -ne 0) { throw "External command failed with code '$LASTEXITCODE'" }
        }
        finally {
            popd 
        }
    }
}

function Show-LabVariable {
    param( 
        [parameter(mandatory=$true)] [string] $varName,
        [switch] $testPath
    )
    $LabVariable = new-object PSObject -Property @{
        Variable = $varName
        Value = get-variable $varName | select -expand value
        PathExists = "-"
    }
    if ($testPath) { $LabVariable.PathExists = test-path $LabVariable.Value }
    return $LabVariable
}

########

#$Basename = Get-Item $MyInvocation.MyCommand.Path | select -expand BaseName
#if ($MyInvocation.InvocationName -match $baseName) { # We were executed from the command line, not dot-sourced

mkdir -force -path $labTempDir | out-null

if ($baseConfigName) {
    write-host ""
    ##write-output "Non-path variables: "
    Show-LabVariable -varName 'fullConfigName'
    ##write-output "`nPaths to files that SHOULD exist already: "
    Show-LabVariable packerConfigRoot -testPath
    Show-LabVariable packerFile -testPath
    Show-LabVariable vagrantTemplate -testPath
    ##write-output "`nPaths to files that SHOULD NOT exist (unless you passed -force): "
    Show-LabVariable outDir -testPath
    Show-LabVariable packedBoxPath -testPath
    write-output ""
}

if ($DownloadWSUS) {
    if (-not (test-path $wsusOfflineDir))  {
        Download-WSUSOfflineUpdater
    }
    Download-WindowsUpdates
}
if ($ApplyWSUS) {
    Apply-WindowsUpdatesToIso -inputIso $isoPath -outputIso $newMediaIsoPath -wsusOfflineDir $wsusOfflineDir -wimMountDir $wimMountDir -verbose:$verbose
}
if ($BuildPacker) {
    $bpfParam = @{
        packerFile = $packerFile
        vagrantTemplate = $vagrantTemplate
        vagrantBoxName = $fullConfigName
        tag = $tag
        packerCacheDir = $packerCacheDir
        outDir = $outDir
        force = $force
        whatIf = $whatIf
    }
    Build-PackerFile @bpfParam
}
if ($AddToVagrant) {
    Add-BoxToVagrant -vagrantBoxName $fullConfigName -packedBoxPath $packedBoxPath -force:$force -whatif:$whatif
}
if ($VagrantUp) {
    Run-VagrantBox -vagrantBoxName $fullConfigName -workingDirectory $outDir -whatif:$whatif
}

#}

