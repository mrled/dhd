[cmdletbinding()]
param(
    [parameter(mandatory=$true)] [string] $packerFile,
    [string] $tag = "", # tag for the tmp dir, so you can keep track of what you were working on
    [switch] $whatIf
)

$errorActionPreference = "Stop"

#$baseTempDir = $env:Temp
$baseOutDir = "D:\iso\vagrants"
$env_packer_cache_dir_backup = $env:PACKER_CACHE_DIR
$env:PACKER_CACHE_DIR = "D:\iso\packer_cache"
set-alias packer (gcm packer | select -expand path)
set-alias vagrant (gcm vagrant | select -expand path)

$startDate = get-date
$packerFilePath = get-item $packerFile | select -expand fullname
$packerFileBaseName = get-item $packerFile | select -expand basename 
$date = get-date -date $startDate -UFormat "%Y-%m-%d-%H-%M-%S"
$vagrantBoxName = "wintriallab-${packerFileBaseName}-${date}"

$outDir = "${baseOutDir}\${vagrantBoxName}"
if ($tag) { $outDir += "-${tag}"}
if (test-path $outDir) { throw "Your `$outDir that already exists at '$outDir'"}

pushd $PSScriptRoot
try { 
    write-host "Building packer file '$packerFilePath' to directory '$outDir'..."
    if (-not $whatif) {
        packer build -var "output_directory=$outDir" "$packerFilePath"
        if ($LASTEXITCODE -ne 0) { 
            throw "Packer appears to have failed :("
        }
    }
}
finally {
    popd
    $env:PACKER_CACHE_DIR = $env_packer_cache_dir_backup
}
$packedBoxPath = get-item $outDir\*.box | select -first 1 -expand fullname
write-host "Packed .box file: '$packedBoxPath'"

if ($vagrantAdd) {
    if (-not $whatIf) {
        vagrant box add --name $vagrantBoxName $packedBoxPath
    }
}