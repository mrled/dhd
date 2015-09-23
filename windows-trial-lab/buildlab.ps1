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
    [parameter(mandatory=$true)] [string] $baseConfigName,
    [parameter(mandatory=$true)] [string[]] $action,
    $baseOutDir = "D:\iso\vagrants",
    $packerCacheDir = "D:\iso\packer_cache",
    [string] $tag,
    [switch] $force,
    [switch] $whatIf
)

$errorActionPreference = "Stop"
#$dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
#$fullConfigName = "wintriallab-${baseConfigName}-${dateStamp}" 
$fullConfigName = "wintriallab-${baseConfigName}" 
set-alias packer (gcm packer | select -expand path)
set-alias vagrant (gcm vagrant | select -expand path)

$outDir = "${baseOutDir}\${fullConfigName}"
if ($tag) { $outDir += "-${tag}"}

$packerConfigRoot = "${PSScriptRoot}\${baseConfigName}"
$packerFile = "${packerConfigRoot}\${baseConfigName}.packerfile.json"
$packedBoxPath = "${outDir}\${baseConfigName}_virtualbox.box"
$vagrantTemplate = "${packerConfigRoot}\vagrantfile-${baseConfigName}.template"

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

# Just always show this: 
#if ($action -contains "Info") {
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
#}

if ($action -contains "BuildPacker") {
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
if ($action -contains "AddToVagrant") {
    Add-BoxToVagrant -vagrantBoxName $fullConfigName -packedBoxPath $packedBoxPath -force:$force -whatif:$whatif
}
if ($action -contains "VagrantUp") {
    Run-VagrantBox -vagrantBoxName $fullConfigName -workingDirectory $outDir -whatif:$whatif
}
