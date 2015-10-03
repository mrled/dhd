[cmdletbinding()] param(
	[parameter(mandatory=$true)] [string] $osArchitecture,
	[parameter(mandatory=$true)] [string] $WindowsVersion,
	[parameter(mandatory=$true)] [string] $isoPath,
	[parameter(mandatory=$true)] [string] $osArchitecture,
	[parameter(mandatory=$true)] [string] $osArchitecture,
)

# This seems to be required with strict mode? 
$verbose = $false
# This correctly covers -verbose -verbose:$false and -verbose:$true
if ($PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent -eq $true) {
    $verbose = $true
}

Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"

get-module slipstream | remove-module
ipmo $PSScriptRoot\slipstream.psm1

$arch = $ArchitectureId.i386
$winver = $WindowsVersionId.w63
$ssTempDir = 'D:\iso\wintriallab\temp-slipstream'
mkdir -force $ssTempDir | out-null
$packageXmlFile = 'D:\iso\wintriallab\wsusscn2\wsusscn2\cabs-extracted\package.xml'
$wuUrlFile = "$ssTempDir\wuUrls.txt"
$wuDownloadCache = "${ssTempDir}\WSUSCache\${winver}-${arch}-glb"
mkdir -force $wuDownloadCache | out-null
		
Get-WindowsUpdateUrls -windowsVersion $winver -osArchitecture $arch -packageXml $packageXmlFile -outFile $wuUrlFile -verbose:$verbose
foreach ($url in (gc $wuUrlFile)) { 
	Get-WebFile -url $url -outDir $wuDownloadCache -verbose:$verbose
}

