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
#function buildlab {
[cmdletbinding()]
param(
    [parameter(mandatory=$true,ParameterSetName="BuildPacker")] 
    [parameter(mandatory=$true,ParameterSetName="AddToVagrant")] 
    [parameter(mandatory=$true,ParameterSetName="VagrantUp")]
    [string] $baseConfigName,

    [parameter(mandatory=$true,ParameterSetName="BuildPacker")]  [switch] $BuildPacker,
    [parameter(mandatory=$true,ParameterSetName="AddToVagrant")] [switch] $AddToVagrant,
    [parameter(mandatory=$true,ParameterSetName="VagrantUp")]    [switch] $VagrantUp,
    
    [parameter(mandatory=$true,ParameterSetName="ShowConfig")]   [switch] $ShowConfig,

    [string] $baseOutDir = "D:\iso\wintriallab",
    [string] $tempDirOverride,
    [string] $tag,
    [switch] $SkipSyntaxcheck,
    [switch] $force,
    [switch] $whatIf
)

$errorActionPreference = "Stop"
Set-StrictMode -Version 2.0

$dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
$packerOutDir = "$baseOutDir\PackerOut"
$packerCacheDir = "$baseOutDir\packer_cache"
$packerLogFile = "$baseOutDir\packer.log"
$wsusOfflineDir = "$baseOutDir\wsusoffline"

$labTempDir = "$baseOutDir\temp-$dateStamp"
if ($tempDirOverride) { $labTempDir = $tempDirOverride }

$wimMountDir = "${labTempDir}\MountInstallWim"
$installMediaTemp = "${labTempDir}\InstallMedia"
$newMediaIsoPath = "${labTempDir}\windows.iso"

$fullConfigName = "wintriallab-${baseConfigName}" 

set-alias packer (gcm packer | select -expand path)
set-alias vagrant (gcm vagrant | select -expand path)


$outDir = "${packerOutDir}\${fullConfigName}"
if ($tag) { $outDir += "-${tag}"}

$packerConfigRoot = "${PSScriptRoot}\packer\${baseConfigName}"
$packerFile = "${packerConfigRoot}\${baseConfigName}.packerfile.json"
$packedBoxPath = "${outDir}\${baseConfigName}_virtualbox.box"
$vagrantTemplate = "${packerConfigRoot}\vagrantfile-${baseConfigName}.template"
    
function Test-PowershellSyntax {
    [cmdletbinding()]
    param(
        [parameter(mandatory=$true)] [string] $fileName,
        [switch] $ThrowOnFailure
    )
    $tokens = @()
    $parseErrors = @()
    $parser = [System.Management.Automation.Language.Parser]
    $fileName = resolve-path $fileName
    $parsed = $parser::ParseFile($fileName, [ref]$tokens, [ref]$parseErrors)

    if ($parseErrors.count -gt 0) {
        $message = "$($parseErrors.count) parse errors found in file '$fileName':`r`n"
        $parseErrors |% { $message += "`r`n    $_" }
        if ($ThrowOnFailure) { throw $message } else { write-verbose $message }
        return $false
    }
    return $true
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
        vagrant box add "$forceOption" --name $vagrantBoxName "$packedBoxPath"    
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
    $varValue = get-variable $varName | select -expand value
    $LabVariable = new-object PSObject -Property @{
        Variable = $varName
        Value = $varValue
        PathExists = if ($testPath) {test-path $varValue} else {"-"}
    }
    return $LabVariable
}

########

#$Basename = Get-Item $MyInvocation.MyCommand.Path | select -expand BaseName
#if ($MyInvocation.InvocationName -match $baseName) { # We were executed from the command line, not dot-sourced

if (-not $SkipSyntaxcheck) {
    foreach ($script in (gci $PSScriptRoot\scripts\* -include *.ps1,*.psm1)) { 
        $valid = Test-PowershellSyntax -fileName $script.fullname
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
    Show-LabVariable outDir -testPath
    Show-LabVariable packedBoxPath -testPath
    write-output ""
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

