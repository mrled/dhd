<#
.synopsis 
Windows Trial lab management script
.parameter baseConfigName
The name of one of the subdirs like "windows_81_x86"
.parameter action
Which build actions do you want to perform? 
.parameter tag
A tag for the temporary directory, the output directory, and the resulting Vagrant box
.notes
PREREQUISITES: 
- packer
- vagrant 1.7.4
- FOR NOW: the winrm-fs vagrant plugin - vagrant plugin install winrm-fs. This should come with the next Vagrant release tho. See: https://github.com/mitchellh/vagrant/issues/6060#issuecomment-130319797
.example
.\buildlab.ps1 -baseConfigName windows_10_x86 -BuildPacker -force
.\buildlab.ps1 -baseConfigName windows_10_x86 -AddToVagrant -force
cd vagrant\FreyjaA
vagrant up
#>
[cmdletbinding()] param(
    [parameter(mandatory=$true,ParameterSetName="BuildPacker")] 
    [parameter(mandatory=$true,ParameterSetName="AddToVagrant")] 
    [parameter(mandatory=$true,ParameterSetName="VagrantUp")]
    [string] $baseConfigName,

    [parameter(mandatory=$true,ParameterSetName="BuildPacker")]  [switch] $BuildPacker,
    [parameter(mandatory=$true,ParameterSetName="AddToVagrant")] [switch] $AddToVagrant,
    [parameter(mandatory=$true,ParameterSetName="VagrantUp")]    [switch] $VagrantUp,
    [parameter(mandatory=$true,ParameterSetName="ShowConfig")]   [switch] $ShowConfig,

    #[string] $baseOutDir = [System.IO.Path]::GetFullPath("$PSScriptRoot\..\..\iso\wintriallab"),
    [string] $baseOutDir = "$env:USERPROFILE\Documents\WinTrialLab",
    [string] $tempDirOverride,
    [string] $tag,
    [switch] $SkipSyntaxcheck,
    [switch] $force,
    [switch] $whatIf
)

$errorActionPreference = "Stop"
if (-not (Get-Module |? -Property Name -match "wintriallab-postinstall")) { import-module $PSScriptRoot\scripts\wintriallab-postinstall.psm1 }

$dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
$packerCacheDir = "$baseOutDir\packer_cache"
$packerLogFile = "$baseOutDir\packer.log"
$wsusOfflineDir = "$baseOutDir\wsusoffline"

$labTempDir = "$baseOutDir\temp-$dateStamp"
if ($tempDirOverride) { $labTempDir = $tempDirOverride }

$wimMountDir = "${labTempDir}\MountInstallWim"
$installMediaTemp = "${labTempDir}\InstallMedia"
$newMediaIsoPath = "${labTempDir}\windows.iso"

$fullConfigName = "wintriallab-${baseConfigName}" 

$packerOutDir = "$baseOutDir\PackerOut\${fullConfigName}"
$vagrantUpDir = "$baseOutDir\VagrantUp\${fullConfigName}"

set-alias packer (gcm packer | select -expand path)
set-alias vagrant (gcm vagrant | select -expand path)

if ($tag) { $packerOutDir += "-${tag}"}

$packerConfigRoot = "${PSScriptRoot}\packer\${baseConfigName}"
$vagrantConfigRoot = "${PSScriptRoot}\vagrant\${baseConfigName}"
$packerFile = "${packerConfigRoot}\${baseConfigName}.packerfile.json"
$packedBoxPath = "${packerOutDir}\${baseConfigName}_virtualbox.box"
$vagrantTemplate = "${packerConfigRoot}\vagrantfile-${baseConfigName}.template"

function Build-PackerFile {
    [cmdletbinding()]
    param(
        [parameter(mandatory=$true)] $packerFile,
        $packerCacheDir,
        $outDir,
        [switch] $force,
        [switch] $whatIf
    )

    $packerFile = get-item $packerFile
    write-host $packerFile
    if ($packerCacheDir) { $env:PACKER_CACHE_DIR = $packerCacheDir }

    if (test-path $outDir) {
        if ($force) { 
            rm -force -recurse $outDir 
        }
        else {
            throw "Outdir already exists at '$outDir'" 
        }
    }

    $packerDir = get-item $packerFile | select -expand fullname | split-path -parent
    if (Test-Path "$packerDir\output-*") {
        if ($force) { 
            rm -recurse -force "$packerDir\output-*"
        }
        else {
            throw "Existing packer tmp dir(s) at '$packerDir\output-*'"
        }
    }
    pushd $packerDir
    try { 
        write-host "Building packer file '$($packerFile.fullname)' to directory '$outDir'..."
        $packerCall = ''
        if (-not $whatif) {
            $env:PACKER_DEBUG = 1
            $env:PACKER_LOG = 1 
            $env:PACKER_PATH = $packerLogFile
            packer build -var "output_directory=$outDir" "$($packerFile.fullname)"
            if ($LASTEXITCODE -ne 0) { throw "External command failed with code $LASTEXITCODE" }
        }
    }
    finally {
        popd
    }
    $outBox = get-item $outDir\*.box
    if ($outBox.PSObject.Properties['count'] -and $outBox.count -gt 1) { 
        throw "Somehow you came up with more than one box here: '$outBox'"
    }
    if ($outBox.fullname -notmatch [Regex]::Escape($packedBoxPath)) { 
        throw "Found an output box '$outBox', but it doesn't match the expected packed box path of '$packedBoxPath'"
    }
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
        $packedBoxDir = Split-Path $packedBoxPath
        $packedBoxName = Split-Path -Leaf $packedBoxPath

        # Had problems until I changed $pwd to be $packedBoxDir and didn't pass vagrant a full path. lol
        Push-Location $packedBoxDir
        $forceOption = ""
        if ($force) { $forceOption = "--force"}
        $vadCmd = "vagrant box add $forceOption --name $vagrantBoxName $packedBoxName"
        write-host -foreground Green $vadCmd
        Invoke-Expression $vadCmd
        Pop-Location

        if ($LASTEXITCODE -ne 0) { throw "External command failed with code '$LASTEXITCODE'" } 
    }
}

function Run-VagrantBox { 
    [cmdletbinding()] param(
    	[parameter(mandatory=$true)] $vagrantBoxName,
        [parameter(mandatory=$true)] $sourceDirectory,  # a subdirectory of the vagrant/ dir
        [parameter(mandatory=$true)] $upDirectory,      # somewhere to copy it to so that vagrant doesn't plop a VM down into the middle of this repo
        [switch] $force,
        [switch] $whatIf
    )
    if (-not $whatIf) {
        $env:VAGRANT_CWD=$sourceDirectory
        write-verbose "Set VAGRANT_CWD environment variable to ${ENV:VAGRANT_CWD}"
        if (test-path $upDirectory) { rm -recurse -force $upDirectory }
        mkdir -force $upDirectory | out-null
        pushd $upDirectory
        write-verbose "Using '$upDirectory' as location for VM"
        vagrant up
        popd 
        if ($LASTEXITCODE -ne 0) { throw "External command failed with code '$LASTEXITCODE'" }
    }
}

function Show-LabVariable {
    param( 
        [parameter(mandatory=$true)] [string] $varName,
        [switch] $testPath
    )
    $varValue = get-variable $varName | select -expand value
    $LabVariable = new-object PSObject -Property @{
        Variable = $varName
        Value = $varValue
        PathExists = if ($testPath) {test-path $varValue} else {"-"}
    }
    write-host $LabVariable
    return $LabVariable
}

########

#$Basename = Get-Item $MyInvocation.MyCommand.Path | select -expand BaseName
#if ($MyInvocation.InvocationName -match $baseName) { # We were executed from the command line, not dot-sourced

if (-not $SkipSyntaxcheck) {
    foreach ($script in (gci $PSScriptRoot\scripts\* -include *.ps1,*.psm1)) { 
        $valid = Test-PowershellSyntax -fileName $script.fullname -ThrowOnFailure
        New-Object PSObject -Property @{
            ScriptName = $script.name
            ValidSyntax = $valid 
        }
    }
}

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
    Show-LabVariable packerOutDir -testPath
    Show-LabVariable packedBoxPath -testPath
    write-output ""
}

if ($BuildPacker) {
    Build-PackerFile -packerFile $packerFile -packerCacheDir $packerCacheDir -outDir $packerOutDir -force:$force -whatif:$whatif
}
if ($AddToVagrant) {
    Add-BoxToVagrant -vagrantBoxName $fullConfigName -packedBoxPath $packedBoxPath -force:$force -whatif:$whatif
}
if ($VagrantUp) {
    Run-VagrantBox -vagrantBoxName $fullConfigName -sourceDirectory $vagrantConfigRoot -upDirectory $vagrantUpDir -whatif:$whatif
}
