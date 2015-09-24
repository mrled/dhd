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

# Module useful for Download-URL at least. TODO: this mixes concerns and may not be ideal?
ipmo $PSScriptRoot\scripts\postinstall\wintriallab-postinstall.psm1 

$dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
$packerOutDir = "$baseOutDir\PackerOut"
$packerCacheDir = "$baseOutDir\packer_cache"
$wsusOfflineDir = "$baseOutDir\wsusoffline"

$labTempDir = "$baseOutDir\temp-$dateStamp"
if ($tempDirOverride) { $labTempDir = $tempDirOverride }

$wimMountDir = "${labTempDir}\MountInstallWim"

$errorActionPreference = "Stop"
#$fullConfigName = "wintriallab-${baseConfigName}-${dateStamp}" 
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
    $exDir = resolve-path "$wsusOfflineDir\.." # why the .. ? because the zipfile puts everything in a 'wsusoffline' folder
    sevenzip x "$dlPath" "-o$exDir"
}
function Download-WindowsUpdates {
    set-alias DownloadUpdates "$wsusOfflineDir\cmd\DownloadUpdates.cmd"
    foreach ($product in @('w63','w63-x64','w100','w100-x64')) {
        DownloadUpdates $product glb /includedotnet /verify 
    }
}

function Test-AdminPrivileges {
    $me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    return $me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
}

function Get-DismWimInfo {
    [cmdletbinding()] param (
        [parameter(mandatory=$true)] [string] $wimfile
    )
    if (-not (Test-AdminPrivileges)) { throw "Admin privileges are required" }
    write-verbose "Getting WIM information for file '$wimFile'"

    $wimFile = resolve-path $wimFile | select -expand Path
    $wimInfoText = Invoke-ExpressionAndCheck "dism /Get-WimInfo /WimFile:`"$wimfile`""

    $wimIndexes = @()
    $currentIndex = $false
    foreach ($line in $wimInfoText) {
        write-verbose $line
        if ($line.startsWith("Index : ")) {
            $currentIndex = New-Object PSObject
            [int]$index = $line -replace "Index : ",""
            Add-Member -inputObject $currentIndex -NotePropertyName "Index" -NotePropertyValue $index
            Add-Member -inputObject $currentIndex -NotePropertyName "WimFile" -NotePropertyValue $wimFile
            Add-Member -inputObject $currentIndex -MemberType ScriptProperty -Name DebugDesc -Value { 
                "$($this.WimFile) / $($this.Index) / $($this.Name)" 
            }
        }
        elseif ([String]::IsNullOrEmpty($line)) {
            if ($currentIndex) { $wimIndexes += @($currentIndex) }
            $currentIndex = $null
        }
        elseif ($currentIndex) {
            $splitLine = [Regex]::Split($line, " : ")
            Add-Member -inputObject $currentIndex -NotePropertyName $splitLine[0] -NotePropertyValue $splitLine[1]
        }
    }
    return $wimIndexes
}

function Apply-WindowsUpdatesToWim {
    [cmdletbinding()] param (
        [parameter(mandatory=$true)] [string] $wimFile,
        [parameter(mandatory=$true)] [string] $wimMountDir,
        [parameter(mandatory=$true)] [string] $winUpdateDir
    )
    write-verbose "Applying Windows Updates to '$wimFile' from '$winUpdateDir'"
    #Unblock-File $installWim
    Set-ItemProperty -path $wimFile -name IsReadOnly -value $false -force 
    foreach ($wimInfo in (Get-DismWimInfo -wimFile $wimFile)) {
        write-verbose "Attempging to apply WSUS Offline Updates to $($wimInfo.DebugDesc)"

        $wimMountSubdir = mkdir "${wimMountDir}\$($wimInfo.Index)" -force | select -expand fullname

        Invoke-ExpressionAndCheck "dism /mount-wim /wimfile:`"$installWim`" /mountdir:`"$wimMountSubdir`" /index:`"$($wimInfo.index)`""

        if ($LASTEXITCODE -and $LASTEXITCODE -ne 0) { throw "External command failed with exit code '$LASTEXITCODE'" }

        ls $updatePath\* -include *.cab |% {
            write-verbose "Applying update at $_ to directory at $wimMountSubdir"
            Invoke-ExpressionAndCheck "dism /image:`"$wimMountSubdir`" /add-package /packagepath:`"$_`""
        }

        Invoke-ExpressionAndCheck "dism /unmount-wim /mountdir:`"$wimMountSubdir`""
    }
}

function Get-BootWimBitness($wimFile) {
    $bootWimInfo = Get-DismWimInfo -wimFile $wimFile
    if (-not $bootWimInfo) { throw "Got no information for wimfile at '$wimFile'"}
    elseif ($bootWimInfo[0].Name -match "x86") { return "i386" }
    elseif ($bootWimInfo[0].Name -match "x64") { return "amd64" }
    else { throw "Could not determine WimBitness"}
}

function Get-WOShortCode($OSName, $OSBitness) {

    $shortCodeTable = @{
        "8.1" = "w63"
    }

    $shortCodeTable.keys |% { if ($OSName -match $_) { $shortCode = $shortCodeTable[$_] } }
    if (-not $shortCode) { throw "Could not determine shortcode for an OS named '$OSName'" }
    write-verbose "Found shortcode '$shortcode' for OS named '$OSName' of bitness '$bitness'"

    if ($OSBitness -match "i386") { $shortCode += "" }
    elseif ($OSBitness -match "amd64") { $shortCode += "-x64" }
    else { throw "Could not determine shortcode for an OS of bitness '$OSBitness'" }

    return $shortCode
}

function Apply-WindowsUpdatesToIso {
    [cmdletbinding()] param (
        [parameter(mandatory=$true)] [string] $iso,
        [parameter(mandatory=$true)] [string] $wsusOfflineDir,
        [parameter(mandatory=$true)] [string] $wimMountDir
    )

    $myWimMounts = @()

    mount-diskimage -imagepath $iso
    $mountedDrive = get-diskimage -imagepath $iso | get-volume | select -expand DriveLetter
    $mountedDrive = get-diskimage -imagepath $iso | get-volume | select -expand DriveLetter

    $installWim = cp "${mountedDrive}:\Sources\install.wim" $labTempDir -passthru | select -expand fullname
    $bitness = Get-BootWimBitness -wimFile "${mountedDrive}:\sources\boot.wim"
    dismount-diskimage -imagepath $iso

    $wimInfo = Get-DismWimInfo -wimFile $installWim
    $shortCode = Get-WOShortCode -OSName $wimInfo[0].Name -bitness $bitness
    $updatePath = resolve-path "${wsusOfflineDir}\client\$shortCode\glb" | select -expand Path

    Apply-WindowsUpdatesToWim -wimFile $installWim -wimMountDir $wimMountDir -winUpdateDir $updatePath

    dismount-diskimage -imagepath $iso
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
    # Doing this different while in development
    Apply-WindowsUpdatesToIso -iso $isoPath -wsusOfflineDir $wsusOfflineDir -wimMountDir $wimMountDir
    <#
    $installWim = resolve-path $labTempDir\install_81_x86.wim | select -expand path
    $wimInfo = Get-DismWimInfo -wimFile $installWim
    $bitness = "i386"
    $shortCode = Get-WOShortCode -OSName $wimInfo[0].Name -OSBitness $bitness
    $updatePath = resolve-path "${wsusOfflineDir}\client\$shortCode\glb" | select -expand Path
    Apply-WindowsUpdatesToWim -Verbose -wimFile $installWim -wimMountDir $wimMountDir -winUpdateDir $updatePath
    #>
    throw "TODO: what about the cmdlets in 'gcm -module dism' ??"
    throw "TODO: to convert this back to an iso, I have to have the WAIK and oscdimg.exe it looks like. Ugh."
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

