param(
    [String] $ScriptProductName = "PostInstall-Marionettist",
    [String] $ScriptPath = $MyInvocation.MyCommand.Path,
    [String] $ScriptName = $MyInvocation.MyCommand.Name
)

<# 
jesus fucking christ
fucking Packer
TODO: 
- make every function 100% reliant on itself only. 
- get rid of calls to Get-LabTempDir
- decided whether I'm using $URLs or not lol
- get better logging - use an Event Log 
#>

### Global Constants that I use elsewhere

$ArchitectureId = @{
    amd64 = "amd64"
    i386 = "i386"
}
$WindowsVersionId = @{
    w63 = "w63" # TODO: rename to w81 probably
    w10 = "w10"
    w10ltsb = "w10ltsb"
    server2012r2 = "server2012r2"
}
$OfficeVersionId = @{
    o2013 = "o2013"
}
$IsoUrls = @{
    $WindowsVersionId.w63 = @{
        $ArchitectureId.i386 =  "http://care.dlservice.microsoft.com/dl/download/B/9/9/B999286E-0A47-406D-8B3D-5B5AD7373A4A/9600.17050.WINBLUE_REFRESH.140317-1640_X86FRE_ENTERPRISE_EVAL_EN-US-IR3_CENA_X86FREE_EN-US_DV9.ISO"
        $ArchitectureId.amd64 = "http://care.dlservice.microsoft.com/dl/download/B/9/9/B999286E-0A47-406D-8B3D-5B5AD7373A4A/9600.17050.WINBLUE_REFRESH.140317-1640_X64FRE_ENTERPRISE_EVAL_EN-US-IR3_CENA_X64FREE_EN-US_DV9.ISO" 
    }
    $WindowsVersionId.w10 = @{
        $ArchitectureId.i386 =  "http://care.dlservice.microsoft.com/dl/download/C/3/9/C399EEA8-135D-4207-92C9-6AAB3259F6EF/10240.16384.150709-1700.TH1_CLIENTENTERPRISEEVAL_OEMRET_X86FRE_EN-US.ISO"
        $ArchitectureId.amd64 = "http://care.dlservice.microsoft.com/dl/download/C/3/9/C399EEA8-135D-4207-92C9-6AAB3259F6EF/10240.16384.150709-1700.TH1_CLIENTENTERPRISEEVAL_OEMRET_X64FRE_EN-US.ISO"
    }
    $WindowsVersionId.w10ltsb = @{
        $ArchitectureId.i386 =  "http://care.dlservice.microsoft.com/dl/download/6/2/4/624ECF83-38A6-4D64-8758-FABC099503DC/10240.16384.150709-1700.TH1_CLIENTENTERPRISE_S_EVAL_X86FRE_EN-US.ISO"
        $ArchitectureId.amd64 = "http://care.dlservice.microsoft.com/dl/download/6/2/4/624ECF83-38A6-4D64-8758-FABC099503DC/10240.16384.150709-1700.TH1_CLIENTENTERPRISE_S_EVAL_X64FRE_EN-US.ISO"
    }
    $WindowsVersionId.server2012r2 = @{
        $ArchitectureId.amd64 = "http://care.dlservice.microsoft.com/dl/download/6/2/A/62A76ABB-9990-4EFC-A4FE-C7D698DAEB96/9600.17050.WINBLUE_REFRESH.140317-1640_X64FRE_SERVER_EVAL_EN-US-IR3_SSS_X64FREE_EN-US_DV9.ISO"
    }
    $OfficeVersionId.o2013 = @{
        $ArchitectureId.i386 =  "http://care.dlservice.microsoft.com/dl/download/2/9/C/29CC45EF-4CDA-4710-9FB3-1489786570A1/OfficeProfessionalPlus_x86_en-us.img"
        $ArchitectureId.amd64 = "http://care.dlservice.microsoft.com/dl/download/2/9/C/29CC45EF-4CDA-4710-9FB3-1489786570A1/OfficeProfessionalPlus_x64_en-us.img"
    }
}
$WSUSCatalogUrl = "http://download.windowsupdate.com/microsoftupdate/v6/wsusscan/wsusscn2.cab"
$WSUSOfflineRepoBaseUrl = "https://svn.wsusoffline.net/svn/wsusoffline/trunk"
$szUrl = "http://7-zip.org/a/$szFilename"
$URLs = @{
    ISOs = @{
        $WindowsVersionId.w63 = @{
            $ArchitectureId.i386 = "http://care.dlservice.microsoft.com/dl/download/B/9/9/B999286E-0A47-406D-8B3D-5B5AD7373A4A/9600.17050.WINBLUE_REFRESH.140317-1640_X86FRE_ENTERPRISE_EVAL_EN-US-IR3_CENA_X86FREE_EN-US_DV9.ISO" 
        } 
    }
    WindowsUpdateCatalog = "http://download.windowsupdate.com/microsoftupdate/v6/wsusscan/wsusscn2.cab"
    WSUSOfflineRepoBase = "https://svn.wsusoffline.net/svn/wsusoffline/trunk"
    SevenZipDownload = @{
        $ArchitectureId.i386 = "http://7-zip.org/a/7z920.msi"
        $ArchitectureId.amd64 = "http://7-zip.org/a/7z920-x64.msi"
    }
    UltraDefragDownload = @{
        $ArchitectureId.i386 =  "http://downloads.sourceforge.net/project/ultradefrag/stable-release/6.1.0/ultradefrag-portable-6.1.0.bin.i386.zip"
        $ArchitectureId.amd64 = "http://downloads.sourceforge.net/project/ultradefrag/stable-release/6.1.0/ultradefrag-portable-6.1.0.bin.amd64.zip"
    }
    SdeleteDownload = "http://download.sysinternals.com/files/SDelete.zip"
}
$script:ScriptPath = $MyInvocation.MyCommand.Path
$script:RestartRegistryKeys = @{
    RunBeforeLogon = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\RunServicesOnce"
    RunAtLogon = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce"
}
$script:RestartRegistryProperty = "$ScriptProductName"
    
### Private support functions I use behind the scenes

function Get-WebUrl {
    [cmdletbinding(DefaultParameterSetName="outDir")] param(
        [parameter(mandatory=$true)] [string] $url,
        [parameter(mandatory=$true,ParameterSetName="outDir")] [string] $outDir,
        [parameter(mandatory=$true,ParameterSetName="outFile")] [string] $outFile
    )
    if ($PScmdlet.ParameterSetName -match "outDir") {
        $filename = [System.IO.Path]::GetFileName($url)
        $outFile = "$outDir\$filename"
    }
    $outFile = [IO.Path]::GetFullPath($outFile)
    (New-Object System.Net.WebClient).DownloadFile($url, $outFile)
    return (get-item $outFile)
}

function Invoke-ExpressionAndCheck {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $command,
        [int] $sleepSeconds
    )
    $global:LASTEXITCODE = 0
    Write-EventLogWrapper "Invoking expression '$command'"
    invoke-expression -command $command
    Write-EventLogWrapper "Expression '$command' had a last exit code of '$LastExitCode'"
    if ($global:LASTEXITCODE -ne 0) {
        throw "LASTEXITCODE: ${global:LASTEXITCODE} for command: '${command}'"
    }
    if ($sleepSeconds) { start-sleep $sleepSeconds }
}

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

function Apply-XmlTransform {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $xmlFile,
        [parameter(mandatory=$true)] [string] $xsltFile,
        [parameter(mandatory=$true)] [string] $outFile
    )
    $xmlFile = resolve-path $xmlFile | select -expand path
    $xsltFile = resolve-path $xsltFile | select -expand path

    if (test-path $outFile) { throw "outFile exists at '$outFile'" }
    $outParent = split-path -parent $outFile | resolve-path | select -expand Path
    $outName = split-path -leaf $outFile
    $outFile = "$outParent\$outName"

    $xslt = New-Object System.Xml.Xsl.XslCompiledTransform
    $xslt.Load($xsltFile)
    $xslt.Transform($xmlFile, $outFile)
    return (get-item $outFile)
}

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

<#
.notes 
For use with WSUS Offline Updater
#>
function Get-WOShortCode { # TODO fixme I think I don't need this anymore because I'm not using WSUS Offline anymore
    param(
        [parameter(mandatory=$true)] [string] $OSName,
        [parameter(mandatory=$true)] [string] $OSArchitecture
    )

    # I'm adding to this list slowly, only as I encounter the actual names from install.wim 
    # on the trial CDs when I actually try to install them
    $shortCodeTable = @{
        "8.1" = "w63"
    }

    $shortCodeTable.keys |% { if ($OSName -match $_) { $shortCode = $shortCodeTable[$_] } }
    if (-not $shortCode) { throw "Could not determine shortcode for an OS named '$OSName'" }

    if ($OSArchitecture -match $ArchitectureId.i386) { $shortCode += "" }
    elseif ($OSArchitecture -match $ArchitectureId.amd64) { $shortCode += "-x64" }
    else { throw "Could not determine shortcode for an OS of architecture '$OSArchitecture'" }

    Write-EventLogWrapper "Found shortcode '$shortcode' for OS named '$OSName' of architecture '$OSArchitecture'"
    return $shortCode
}


### Publicly exported functions called directly from slipstreaming scripts

<#
.notes
This is intended for use in the postinstall phase, on the target machine
We expect calling scripts to get a lab temp dir with this function, but we do NOT permit functions in the module to call it
TODO: does that even make sense to do??
Only functions that were intended to run in that phase should have the concept of a "LabTempDir".
NOTE: this will return the same directory every time it's called until the module is reimported
#>
function Get-LabTempDir {
    if ("${script:WinTrialLabTemp}") {} # noop
    elseif ("${env:WinTrialLabTemp}") {
        $script:WinTrialLabTemp = $env:WinTrialLabTemp
    }
    else {
        $dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
        $script:WinTrialLabTemp = "${env:Temp}\WinTrialLab-$dateStamp" 
    }
    $script:WinTrialLabTemp = [System.IO.Path]::GetFullPath($script:WinTrialLabTemp)
    Write-EventLogWrapper "Using WinTrialLabTemp directory at '${script:WinTrialLabTemp}'"
    if (-not (test-path $script:WinTrialLabTemp)) {
        Write-EventLogWrapper "Temporary directory does not exist, creating it..."
        mkdir -force $script:WinTrialLabTemp | out-null
    }
    return $script:WinTrialLabTemp
}

<#
.synopsis 
Wrapper that writes to the event log but also to the screen
#>
function Write-EventLogWrapper {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [String] $message,
        [int] $eventId = 0,
        [ValidateSet("Error",'Warning','Information','SuccessAudit','FailureAudit')] $entryType = "Information",
        [String] $EventLogName = $ScriptProductName,
        [String] $EventLogSource = $ScriptName
    )
    if (-not (get-eventlog -logname * |? { $_.Log -eq $eventLogName })) {
        New-EventLog -Source $EventLogSource -LogName $eventLogName
    }
    $messagePlus = "$message`r`n`r`nScript: $($script:ScriptPath)`r`nUser: ${env:USERDOMAIN}\${env:USERNAME}"
    Write-Host -foreground magenta "====Writing to $EvengLogName event log===="
    write-host -foreground darkgray "$messagePlus`r`n"
    Write-EventLog -LogName $eventLogName -Source $EventLogSource -EventID $eventId -EntryType $entryType -Message $MessagePlus
}

<#
.synopsis
Create and set the registry property which will run this script on reboot 
#>
function Set-RestartRegistryEntry {
    param(
        [parameter(mandatory=$true)] [ValidateSet('RunBeforeLogon','RunAtLogon','NoRestart')] [string] $RestartAction,
        [string] $restartCommand
    )

    if ($RestartAction -match "NoRestart") {
        Write-EventLogWrapper "Called Set-RestartRegistryEntry with -RestartAction NoRestart, will not write registry key" 
        return 
    }
    
    $message = "Setting the Restart Registry Key at: {0}\{1}`r`n{2}" -f $script:RestartRegistryKeys.$RestartAction, $script:RestartRegistryProperty, $restartCommand
    Write-EventLogWrapper -message $message
    New-Item $script:RestartRegistryKeys.$RestartAction -force | out-null
    Set-ItemProperty -Path $script:RestartRegistryKeys.$RestartAction -Name $script:RestartRegistryProperty -Value $restartCommand
}

function Get-RestartRegistryEntries {
    [cmdletbinding()] param()
    foreach ($key in $script:RestartRegistryKeys.Keys) { 
        try { Get-ItemProperty -Path $script:RestartRegistryKeys[$key] -name $script:RestartRegistryProperty} catch {}
    }
}

function Remove-RestartRegistryEntries {
    [cmdletbinding()] param()
    foreach ($key in $script:RestartRegistryKeys.Keys) { 
        try { Remove-ItemProperty -Path $script:RestartRegistryKeys[$key] -name $script:RestartRegistryProperty} catch {}
    }
}

<#
.description
Return the OS Architecture of the current system, as determined by WMI
Will return either "i386" or "amd64"
TODO: this isn't a great method but I'm tired of trying to find the totally correct one. This one isn't ideal because OSArchitecture can be localized. 
I've seen some advice that you should call into the registry  
- reg Query "HKLM\Hardware\Description\System\CentralProcessor\0" | find /i "x86" > NUL && set OSARCHITECTURE=32BIT || set OSARCHITECTURE=64BIT
- http://stackoverflow.com/a/24590583/868206
- https://support.microsoft.com/en-us/kb/556009
... however, this lets you know about the HARDWARE, not the OPERATING SYSTEM - we care about the latter
#>
function Get-OSArchitecture {
    $OSArch = Get-WmiObject -class win32_operatingsystem -property osarchitecture | select -expand OSArchitecture
    if ($OSArch -match "64") { return $ArchitectureId.amd64 } 
    elseif ($OSArch -match "32") { return $ArchitectureId.i386 }
    else { throw "Could not determine OS Architecture from string '$OSArch'" }
}

function Test-AdminPrivileges {
    [cmdletbinding()] param(
        [switch] $ThrowIfNotElevated
    )
    $me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    $elevated = $me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
    if ($ThrowIfNotElevated -and (! $elevated)) { throw "Administrative privileges are required" }
    return $elevated
}

function Install-SevenZip { 
    $OSArch = Get-OSArchitecture
    $szDlPath = Get-WebUrl -url $URLs.SevenZipDownload.$OSArch -outDir $env:temp
    try {
        Write-EventLogWrapper "Downloaded '$szUrl' to '$szDlPath', now running msiexec..."    
        $msiCall = '& msiexec /qn /i "{0}"' -f $szDlPath
        # Windows suxxx so msiexec sometimes returns right away? or something idk. fuck
        Invoke-ExpressionAndCheck -command $msiCall -sleepSeconds 30
    }
    finally {
        rm -force $szDlPath
    }
}
set-alias sevenzip "${env:ProgramFiles}\7-Zip\7z.exe"

function Install-VBoxAdditions {
    [cmdletbinding(DefaultParameterSetName="InstallFromDisc")] param(
        [parameter(ParameterSetName="InstallFromIsoPath",mandatory=$true)] [string] $isoPath,
        [parameter(ParameterSetName="InstallFromDisc",mandatory=$true)] [switch] $fromDisc
    )
        
    function InstallVBoxAdditionsFromDir {
        param([Parameter(Mandatory=$true)][String]$baseDir)
        $baseDir = resolve-path $baseDir | select -expand Path
        Write-EventLogWrapper "Installing VBox Additions from '$baseDir'"
        Write-EventLogWrapper "Installing the Oracle certificate..."
        $oracleCert = resolve-path "$baseDir\cert\oracle-vbox.cer" | select -expand path
        # NOTE: Checking for exit code, but this command will fail with an error if the cert is already installed
        Invoke-ExpressionAndCheck -command ('& "{0}" add-trusted-publisher "{1}" --root "{1}"' -f "$baseDir\cert\VBoxCertUtil.exe",$oracleCert)
        Write-EventLogWrapper "Installing the virtualbox additions"
        Invoke-ExpressionAndCheck -command ('& "{0}" /with_wddm /S' -f "$baseDir\VBoxWindowsAdditions.exe") # returns IMMEDIATELY, goddamn fuckers
        while (get-process -Name VBoxWindowsAdditions*) { write-host 'Waiting for VBox install to finish...'; sleep 1; }
        Write-EventLogWrapper "virtualbox additions have now been installed"
    }
    
    switch ($PSCmdlet.ParameterSetName) {
        "InstallFromIsoPath" {
            $isoPath = resolve-path $isoPath | select -expand Path
            $vbgaPath = mkdir -force "${env:Temp}\InstallVbox" | select -expand fullname
            try {
                Write-EventLogWrapper "Extracting iso at '$isoPath' to directory at '$vbgaPath'..."
                Invoke-ExpressionAndCheck -command ('sevenzip x "{0}" -o"{1}"' -f $isoPath, $vbgaPath)
                InstallVBoxAdditionsFromDir $vbgaPath
            }
            finally {
                rm -recurse -force $vbgaPath
            }
        }
        "InstallFromDisc" {
            $vboxDiskDrive = get-psdrive -PSProvider Filesystem |? { test-path "$($_.Root)\VBoxWindowsAdditions.exe" }
            if ($vboxDiskDrive) { 
                Write-EventLogWrapper "Found VBox Windows Additions disc at $vboxDiskDrive"
                InstallVBoxAdditionsFromDir $vboxDiskDrive.Root
            }
            else {
                $message = "Could not find VBox Windows Additions disc"
                Write-EventLogWrapper $message
                throw $message
            }
        }
    }
}

function Disable-AutoAdminLogon {
    Write-EventLogWrapper "Disabling auto admin logon"
    set-itemproperty -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon" -Name AutoAdminLogon -Value 0    
}

function Enable-RDP { # TODO fixme
    netsh advfirewall firewall add rule name="Open Port 3389" dir=in action=allow protocol=TCP localport=3389
    reg add "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Terminal Server" /v fDenyTSConnections /t REG_DWORD /d 0 /f
}

function Install-CompiledDotNetAssemblies {
    # http://support.microsoft.com/kb/2570538
    # http://robrelyea.wordpress.com/2007/07/13/may-be-helpful-ngen-exe-executequeueditems/

    $ngen = "${env:WinDir}\microsoft.net\framework\v4.0.30319\ngen.exe"
    Invoke-ExpressionAndCheck -command "& $ngen update /force /queue"
    Invoke-ExpressionAndCheck -command "& $ngen executequeueditems"
        
    if ((Get-OSArchitecture) -match $ArchitectureId.amd64) { 
        $ngen64 = "${env:WinDir}\microsoft.net\framework64\v4.0.30319\ngen.exe"
        Invoke-ExpressionAndCheck -command "& $ngen64 update /force /queue"
        Invoke-ExpressionAndCheck -command "& $ngen64 executequeueditems"
    }
}

function Compress-WindowsInstall {
    $OSArch = Get-OSArchitecture
    try {
        $udfZipPath = Get-WebUrl -url $URLs.UltraDefragDownload.$OSArch -outDir $env:temp
        $udfExPath = "${env:temp}\ultradefrag-portable-6.1.0.$OSArch"
        # This archive contains a folder - extract it directly to the temp dir
        Invoke-ExpressionAndCheck -command ('sevenzip x "{0}" "-o{1}"' -f $udfZipPath,$env:temp)

        $sdZipPath = Get-WebUrl -url $URLs.SdeleteDownload -outDir $env:temp
        $sdExPath = "${env:temp}\SDelete"
        # This archive does NOT contain a folder - extract it to a subfolder (will create if necessary)
        Invoke-ExpressionAndCheck -command ('sevenzip x "{0}" "-o{1}"' -f $sdZipPath,$sdExPath)

        stop-service wuauserv
        rm -recurse -force ${env:WinDir}\SoftwareDistribution\Download
        start-service wuauserv

        Invoke-ExpressionAndCheck -command ('& {0} --optimize --repeat "{1}"' -f "$udfExPath\udefrag.exe","$env:SystemDrive")
        
        $sdKey = "HKCU:\Software\Sysinternals\SDelete"
        if (-not (test-path $sdKey)) { New-Item $sdKey -Force }
        Set-ItemProperty -path $sdKey -name EulaAccepted -value 1 
        Invoke-ExpressionAndCheck -command ('& {0} -q -z "{1}"' -f "$sdExPath\SDelete.exe",$env:SystemDrive)
    }
    finally {
        rm -recurse -force $udfZipPath,$udfExPath,$sdZipPath,$sdExPath -ErrorAction Continue
    }
}

function Disable-WindowsUpdates {
    Test-AdminPrivileges -ThrowIfNotElevated

    $Updates = (New-Object -ComObject "Microsoft.Update.AutoUpdate").Settings
    if ($Updates.ReadOnly) { 
        throw "Cannot update Windows Update settings due to GPO restrictions." 
    }

    $Updates.NotificationLevel = 1 # 1 = Disabled lol
    $Updates.Save()
    $Updates.Refresh()
}

function Enable-MicrosoftUpdate {
    [cmdletbinding()] param()
    Write-EventLogWrapper "Enabling Microsoft Update..."
    stop-service wuauserv
    $auKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update" 
    Set-ItemProperty -path $auKey -name EnableFeaturedSoftware -value 1 
    Set-ItemProperty -path $auKey -name IncludeRecommendedUpdates -value 1 

    $ServiceManager = New-Object -ComObject "Microsoft.Update.ServiceManager"
    $ServiceManager.AddService2("7971f918-a847-4430-9279-4a52d1efe18d",7,"") | out-null

    start-service wuauserv
}

function Install-Chocolatey {
    [cmdletbinding()] param()
    
    $chocoExePath = "${env:ProgramData}\Chocolatey\bin"
    if ($($env:Path).ToLower().Contains($($chocoExePath).ToLower())) {
        Write-EventLogWrapper "Attempting to install Chocolatey but it's already in path, exiting..."
        return
    }

    $systemPath = [Environment]::GetEnvironmentVariable('Path', [System.EnvironmentVariableTarget]::Machine)
    $systemPath += ";$chocoExePath"
    [Environment]::SetEnvironmentVariable("PATH", $systemPath, [System.EnvironmentVariableTarget]::Machine)

    $env:Path = $systemPath
    $userPath = [Environment]::GetEnvironmentVariable('Path', [System.EnvironmentVariableTarget]::User)
    if ($userPath) { $env:Path += ";$userPath" }

    # TODO: capture and log output
    $chocoOutput = iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
    Write-EventLogWrapper "Chocolatey install process completed:`r`n`r`n$chocoOutput"
}

function Set-UserOptions {
    [cmdletbinding()] param(
        [switch] $ShowHiddenFiles,
        [switch] $ShowSystemFiles,
        [switch] $ShowFileExtensions,
        [switch] $ShowStatusBar,
        [switch] $DisableSharingWizard,
        [switch] $EnablePSOnWinX,
        [switch] $EnableQuickEdit
    )
    $explorerAdvancedKey = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced'
    if ($ShowHiddenFiles)      { Set-ItemProperty -path $explorerAdvancedKey -name Hidden -value 1 }
    if ($ShowSystemFiles)      { Set-ItemProperty -path $explorerAdvancedKey -name ShowSuperHidden -value 1 }
    if ($ShowFileExtensions)   { Set-ItemProperty -path $explorerAdvancedKey -name HideFileExt -value 0 }
    if ($ShowStatusBar)        { Set-ItemProperty -path $explorerAdvancedKey -name ShowStatusBar -value 1 }
    if ($DisableSharingWizard) { Set-ItemProperty -path $explorerAdvancedKey -name SharingWizardOn -value 0 }
    if ($EnablePSOnWinX)       { Set-ItemProperty -path $explorerAdvancedKey -name DontUsePowerShellOnWinX -value 0 }

    $consoleKey = "HKCU:\Console"
    if ($EnableQuickEdit) { Set-ItemProperty -path $consoleKey -name QuickEdit -value 1 }
}

function Disable-HibernationFile {
    [cmdletbinding()] param()
    Write-EventLogWrapper "Removing Hibernation file..."
    $powerKey = 'HKLM:\SYSTEM\CurrentControlSet\Control\Power'
    Set-ItemProperty -path $powerKey -name HibernateFileSizePercent -value 0  # hiberfil is zero bytes
    Set-ItemProperty -path $powerKey -name HibernateEnabled -value 0          # disable hibernation altogether
}

function Enable-WinRM { # TODO fixme, would prefer this to be in Powershell if possible. also it's totally insecure
    [cmdletbinding()] param()
    Write-EventLogWrapper "Enabling WinRM..."

    cmd /c winrm quickconfig -q
    cmd /c winrm quickconfig -transport:http
    cmd /c winrm set winrm/config @{MaxTimeoutms="1800000"}
    cmd /c winrm set winrm/config/winrs @{MaxMemoryPerShellMB="1800"}
    cmd /c winrm set winrm/config/service @{AllowUnencrypted="true"}
    cmd /c winrm set winrm/config/service/auth @{Basic="true"}
    cmd /c winrm set winrm/config/client/auth @{Basic="true"}
    cmd /c winrm set winrm/config/listener?Address=*+Transport=HTTP @{Port="5985"} 
    cmd /c netsh advfirewall firewall set rule group="remote administration" new enable=yes 
    cmd /c netsh firewall add portopening TCP 5985 "Port 5985" 
    cmd /c net stop winrm 
    cmd /c sc config winrm start= auto
    cmd /c net start winrm

<#
    # cmd /c winrm quickconfig -q
    # cmd /c winrm quickconfig -transport:http
    Enable-PSRemoting –force -SkipNetworkProfileCheck

    set-item wsman:\localhost\MaxTimeoutms -value 1800000 -force
    set-item wsman:\localhost\Shell\MaxMemoryPerShellMB -value 1800 -force
    set-item wsman:\localhost\Service\AllowUnencrypted -value $true -force
    set-item wsman:\localhost\Service\Auth\Basic -value $true -force
    set-item wsman:\localhost\Client\Auth\Basic -value $true -force

    # cmd /c winrm set winrm/config/listener?Address=*+Transport=HTTP @{Port="5985"} 
    $httpListener = $null
    foreach ($listener in (ls WSMan:\localhost\Listener)) {
        foreach ($key in $listener.keys) {
            $subKey = $key.split("=")[0]
            $subVal = $key.split("=")[1]
            if (($subKey -match "Transport") -and ($subVal = "HTTP")) {
                $httpListener = $listener
                break
            }
        }
        if ($httpListener) { break }
    }
    set-item $httpListener -value 5894

    Set-Item wsman:\localhost\client\auth\CredSSP -value true -force 
    Enable-WSManCredSSP -force -role server -force
    #set-item wsman:localhost\client\trustedhosts -value * -force
    #set-item wsman:localhost\client\trustedhosts -value 1.1.2.242 
    set-item wsman:\localhost\listener\listener*\port -value 81 -force

    stop-service winrm 
    set-service -StartupType "automatic"
    start-service winrm

    winrm get winrm/config 
    winrm enumerate winrm/config/listener 
    #>
}

function Set-PasswordExpiry { # TODO fixme use pure Powershell
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $accountName,
        [parameter(mandatory=$true)] [bool] $expirePassword
    )
    $pe = "TRUE"
    if (-not $expirePassword) { $pe = "FALSE" }
    cmd.exe /c wmic useraccount where "name='$accountName'" set "PasswordExpires=$pe"
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

function Get-WindowsUpdateUrls { # TODO: is this how we wanna do temps tho?
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] $windowsVersion,
        [parameter(mandatory=$true)] $osArchitecture,
        [parameter(mandatory=$true)] $packageXml,
        [parameter(mandatory=$true)] $outFile,
        [switch] $debugSaveXslt
    )
    $xsltPath = [IO.Path]::GetTempFileName()
    Write-EventLogWrapper "Downloading XSLT to '$xsltPath'"
        
    if ($osArchitecture -match $ArchitectureId.i386) { $arch = "x86" }
    elseif ($osArchitecture -match $ArchitectureId.amd64) { $arch = "x64" }
    else { throw "Dunno bout architecture '$osArchitecture'" }
    
    $xsltUrl = "$WSUSOfflineRepoBaseUrl/xslt/ExtractDownloadLinks-$windowsVersion-$arch-glb.xsl"
    Get-WebFile -url $xsltUrl -outFile $xsltPath
    
    Apply-XmlTransform -xmlFile $packageXml -xsltFile $xsltPath -outFile $outFile
    if (-not $debugSaveXslt) { rm -force $xsltPath }
    
    return $outFile
}


# Exports: #TODO
$emmParams = @{
    Alias = @("sevenzip")
    Variable = @("ArchitectureId")
    Function = "*"
    # Function =  @(
    #     "Get-OSArchitecture"
    #     "Get-LabTempDir"
    #     "Install-SevenZip"
    #     "Install-VBoxAdditions"
    # )
}
export-modulemember @emmParams
