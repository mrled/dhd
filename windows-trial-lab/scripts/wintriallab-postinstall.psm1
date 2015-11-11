param(
    [String] $ScriptProductName = "PostInstall-Marionettist",
    [String] $ScriptPath = $MyInvocation.MyCommand.Path,
    [String] $ScriptName = $MyInvocation.MyCommand.Name
)

### Global Constants that I use elsewhere

$ArchitectureId = @{
    amd64 = "amd64"
    i386 = "i386"
}
$WindowsVersionId = @{
    w81 = "w81" 
    w10 = "w10"
    w10ltsb = "w10ltsb"
    server2012r2 = "server2012r2"
}

$URLs = @{
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

<#
.synopsis
Invoke an expression; log the expression, any output, and the last exit code 
#>
function Invoke-ExpressionAndLog {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $command,
        [switch] $invokeWithCmdExe,
        [switch] $checkExitCode,
        [int] $sleepSeconds
    )
    $global:LASTEXITCODE = 0
    if ($invokeWithCmdExe) {
        Write-EventLogWrapper "Invoking CMD: '$command'"
        $output = cmd /c "$command"
    }
    else { 
        Write-EventLogWrapper "Invoking Powershell expression: '$command'"
        $output = invoke-expression -command $command
    }
    Write-EventLogWrapper "Expression '$command' had a last exit code of '$LastExitCode' and output the following to the console:`r`n`r`n$output"
    if ($checkExitCode -and $global:LASTEXITCODE -ne 0) {
        throw "LASTEXITCODE: ${global:LASTEXITCODE} for command: '${command}'"
    }
    if ($sleepSeconds) { start-sleep $sleepSeconds }
}

### Publicly exported functions called directly from slipstreaming scripts

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
Invoke a scriptblock. If it throws, write the errors out to the event log and exist with an error code
.notes
This is intended to be a handy wrapper for calling functions in this module that takes care of logging an exception for you. 
See the autounattend-postinstall.ps1 and provisioner-postinstall.ps1 scripts for examples.
#>
function Invoke-ScriptblockAndCatch {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [ScriptBlock] $scriptBlock,
        [int] $failureExitCode = 666
    )
    try {
        Invoke-Command $scriptBlock
    }
    catch {
        Write-ErrorStackToEventLog -errorStack $_ 
        exit $failureExitCode
    }
}

function Write-ErrorStackToEventLog {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] $errorStack
    )
    $message  = "======== CAUGHT EXCEPTION ========`r`n$errorStack`r`n"
    $message += "======== ERROR STACK ========`r`n"
    $errorStack |% { $message += "$_`r`n----`r`n" }
    $message += "======== ========"
    Write-EventLogWrapper $message
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

function Get-RestartRegistryEntry {
    [cmdletbinding()] param()
    $entries = @()
    foreach ($key in $script:RestartRegistryKeys.Keys) {
        $regKey = $script:RestartRegistryKeys[$key]
        $entry = New-Object PSObject -Property @{
            RegistryKey = $regKey
            RegistryProperty = $script:RestartRegistryProperty
            PropertyValue = $null
        }
        if (test-path $regKey) {
            $regProps = get-item $regKey | select -expand Property  
            if ($regProps -contains $script:RestartRegistryProperty) { 
                $entry.PropertyValue = Get-ItemProperty -path $regKey | select -expand $script:RestartRegistryProperty  
            }
        }
        Add-Member -inputObject $entry -memberType ScriptProperty -Name StringRepr -Value {
            "Key: $($this.RegistryKey), Property: $($this.RegistryProperty), Value: $($this.PropertyValue)"
        }
        $entries += @($entry) 
    }
    return $entries
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
        Write-EventLogWrapper "Downloaded '$($URLs.SevenZipDownload.$OSArch)' to '$szDlPath', now running msiexec..."    
        $msiCall = '& msiexec /qn /i "{0}"' -f $szDlPath
        # Windows suxxx so msiexec sometimes returns right away? or something idk. fuck
        Invoke-ExpressionAndLog -checkExitCode -command $msiCall -sleepSeconds 30
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
        Invoke-ExpressionAndLog -checkExitCode -command ('& "{0}" add-trusted-publisher "{1}" --root "{1}"' -f "$baseDir\cert\VBoxCertUtil.exe",$oracleCert)
        Write-EventLogWrapper "Installing the virtualbox additions"
        Invoke-ExpressionAndLog -checkExitCode -command ('& "{0}" /with_wddm /S' -f "$baseDir\VBoxWindowsAdditions.exe") # returns IMMEDIATELY, goddamn fuckers
        while (get-process -Name VBoxWindowsAdditions*) { write-host 'Waiting for VBox install to finish...'; sleep 1; }
        Write-EventLogWrapper "virtualbox additions have now been installed"
    }
    
    switch ($PSCmdlet.ParameterSetName) {
        "InstallFromIsoPath" {
            $isoPath = resolve-path $isoPath | select -expand Path
            $vbgaPath = mkdir -force "${env:Temp}\InstallVbox" | select -expand fullname
            try {
                Write-EventLogWrapper "Extracting iso at '$isoPath' to directory at '$vbgaPath'..."
                Invoke-ExpressionAndLog -checkExitCode -command ('sevenzip x "{0}" -o"{1}"' -f $isoPath, $vbgaPath)
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

function Enable-RDP {
    Write-EventLogWrapper "Enabling RDP"
    netsh advfirewall firewall add rule name="Open Port 3389" dir=in action=allow protocol=TCP localport=3389
    reg add "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Terminal Server" /v fDenyTSConnections /t REG_DWORD /d 0 /f
}

function Install-CompiledDotNetAssemblies {
    # http://support.microsoft.com/kb/2570538
    # http://robrelyea.wordpress.com/2007/07/13/may-be-helpful-ngen-exe-executequeueditems/
    # Don't check the return value - sometimes it fails and that's fine

    set-alias ngen32 "${env:WinDir}\microsoft.net\framework\v4.0.30319\ngen.exe"
    ngen32 update /force /queue
    ngen32 executequeueditems
        
    if ((Get-OSArchitecture) -match $ArchitectureId.amd64) { 
        set-alias ngen64 "${env:WinDir}\microsoft.net\framework64\v4.0.30319\ngen.exe"
        ngen64 update /force /queue
        ngen64 executequeueditems
    }
}

function Compress-WindowsInstall {
    $OSArch = Get-OSArchitecture
    try {
        $udfZipPath = Get-WebUrl -url $URLs.UltraDefragDownload.$OSArch -outDir $env:temp
        $udfExPath = "${env:temp}\ultradefrag-portable-6.1.0.$OSArch"
        # This archive contains a folder - extract it directly to the temp dir
        Invoke-ExpressionAndLog -checkExitCode -command ('sevenzip x "{0}" "-o{1}"' -f $udfZipPath,$env:temp)

        $sdZipPath = Get-WebUrl -url $URLs.SdeleteDownload -outDir $env:temp
        $sdExPath = "${env:temp}\SDelete"
        # This archive does NOT contain a folder - extract it to a subfolder (will create if necessary)
        Invoke-ExpressionAndLog -checkExitCode -command ('sevenzip x "{0}" "-o{1}"' -f $sdZipPath,$sdExPath)

        stop-service wuauserv
        rm -recurse -force ${env:WinDir}\SoftwareDistribution\Download
        start-service wuauserv

        Invoke-ExpressionAndLog -checkExitCode -command ('& {0} --optimize --repeat "{1}"' -f "$udfExPath\udefrag.exe","$env:SystemDrive")
        Invoke-ExpressionAndLog -checkExitCode -command ('& {0} /accepteula -q -z "{1}"' -f "$sdExPath\SDelete.exe",$env:SystemDrive)
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
        [switch] $EnableQuickEdit,
        [switch] $DisableSystrayHide,
        [switch] $DisableIEFirstRunCustomize
    )
    $explorerAdvancedKey = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced'
    if ($ShowHiddenFiles)      { Set-ItemProperty -path $explorerAdvancedKey -name Hidden -value 1 }
    if ($ShowSystemFiles)      { Set-ItemProperty -path $explorerAdvancedKey -name ShowSuperHidden -value 1 }
    if ($ShowFileExtensions)   { Set-ItemProperty -path $explorerAdvancedKey -name HideFileExt -value 0 }
    if ($ShowStatusBar)        { Set-ItemProperty -path $explorerAdvancedKey -name ShowStatusBar -value 1 }
    if ($DisableSharingWizard) { Set-ItemProperty -path $explorerAdvancedKey -name SharingWizardOn -value 0 }
    if ($EnablePSOnWinX)       { Set-ItemProperty -path $explorerAdvancedKey -name DontUsePowerShellOnWinX -value 0 }
    
    $explorerKey = 'HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer'
    if ($DisableSystrayHide)   { Set-ItemProperty -path $explorerKey -name EnableAutoTray -value 0 }

    $consoleKey = "HKCU:\Console"
    if ($EnableQuickEdit) { Set-ItemProperty -path $consoleKey -name QuickEdit -value 1 }
    
    $internetExplorerKey = "HKCU:\Software\Policies\Microsoft\Internet Explorer\Main"
    if ($DisableIEFirstRunCustomize) { Set-ItemProperty -path $internetExplorerKey -name DisableFirstRunCustomize -value 1 }
}

<#  
.SYNOPSIS  
This function are used to pin and unpin programs from the taskbar and Start-menu in Windows 7 and Windows Server 2008 R2 
.DESCRIPTION  
The function have to parameteres which are mandatory: 
Action: PinToTaskbar, PinToStartMenu, UnPinFromTaskbar, UnPinFromStartMenu 
FilePath: The path to the program to perform the action on 
.notes
from: https://gallery.technet.microsoft.com/scriptcenter/b66434f1-4b3f-4a94-8dc3-e406eb30b750
TODO: I hate it when things pollute the global variable space!
.EXAMPLE 
Set-PinnedApplication -Action PinToTaskbar -FilePath "C:\WINDOWS\system32\notepad.exe" 
.EXAMPLE 
Set-PinnedApplication -Action UnPinFromTaskbar -FilePath "C:\WINDOWS\system32\notepad.exe" 
#>  
function Set-PinnedApplication { 
    [CmdletBinding()] param( 
        [Parameter(Mandatory=$true)][string]$Action,  
        [Parameter(Mandatory=$true)][string]$FilePath 
    ) 
    if (-not (test-path $FilePath)) { throw "No file at '$FilePath'" }   
    
    function InvokeVerb { 
        param([string]$FilePath,$verb) 
        $verb = $verb.Replace("&","") 
        $path = split-path $FilePath 
        $shell = new-object -com "Shell.Application"  
        $folder = $shell.Namespace($path)    
        $item = $folder.Parsename((split-path $FilePath -leaf)) 
        $itemVerb = $item.Verbs() | ? {$_.Name.Replace("&","") -eq $verb} 
        if ($itemVerb) { $itemVerb.DoIt() } else { throw "Verb $verb not found." }
    } 
    function GetVerb { 
        param([int]$verbId) 
        try { $t = [type]"CosmosKey.Util.MuiHelper" }
        catch { 
            $def = [Text.StringBuilder]"" 
            [void]$def.AppendLine('[DllImport("user32.dll")]') 
            [void]$def.AppendLine('public static extern int LoadString(IntPtr h,uint id, System.Text.StringBuilder sb,int maxBuffer);') 
            [void]$def.AppendLine('[DllImport("kernel32.dll")]') 
            [void]$def.AppendLine('public static extern IntPtr LoadLibrary(string s);') 
            add-type -MemberDefinition $def.ToString() -name MuiHelper -namespace CosmosKey.Util             
        } 
        if($global:CosmosKey_Utils_MuiHelper_Shell32 -eq $null){         
            $global:CosmosKey_Utils_MuiHelper_Shell32 = [CosmosKey.Util.MuiHelper]::LoadLibrary("shell32.dll") 
        } 
        $maxVerbLength = 255
        $verbBuilder = new-object Text.StringBuilder "",$maxVerbLength
        [void][CosmosKey.Util.MuiHelper]::LoadString($CosmosKey_Utils_MuiHelper_Shell32,$verbId,$verbBuilder,$maxVerbLength)
        return $verbBuilder.ToString()
    }
    $verbs = @{
        "PintoStartMenu"=5381
        "UnpinfromStartMenu"=5382
        "PintoTaskbar"=5386
        "UnpinfromTaskbar"=5387
    }
    if ($verbs.$Action -eq $null) { 
        throw "Action $action not supported`nSupported actions are:`n`tPintoStartMenu`n`tUnpinfromStartMenu`n`tPintoTaskbar`n`tUnpinfromTaskbar"
    }
    InvokeVerb -FilePath $FilePath -Verb $(GetVerb -VerbId $verbs.$action)
}

function Disable-HibernationFile {
    [cmdletbinding()] param()
    Write-EventLogWrapper "Removing Hibernation file..."
    $powerKey = 'HKLM:\SYSTEM\CurrentControlSet\Control\Power'
    Set-ItemProperty -path $powerKey -name HibernateFileSizePercent -value 0  # hiberfil is zero bytes
    Set-ItemProperty -path $powerKey -name HibernateEnabled -value 0          # disable hibernation altogether
}

<#
.synopsis
Forcibly enable WinRM 
.notes 
TODO: Rewrite in pure Powershell 
#>
function Enable-WinRM {
    [cmdletbinding()] param()
    Write-EventLogWrapper "Enabling WinRM..."

    # I've had the best luck doing it this way - NOT doing it in a single batch script
    # Sometimes one of these commands will stop further execution in a batch script, but when I 
    # call cmd.exe over and over like this, that problem goes away. 
    # Note: order is important. This order makes sure that any time packer can successfully 
    # connect to WinRm, it won't later turn winrm back off or make it unavailable.
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'net stop winrm'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'sc.exe config winrm start= auto'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm quickconfig -q'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm quickconfig -transport:http'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config @{MaxTimeoutms="1800000"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/winrs @{MaxMemoryPerShellMB="2048"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/service @{AllowUnencrypted="true"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/client @{AllowUnencrypted="true"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/service/auth @{Basic="true"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/client/auth @{Basic="true"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/service/auth @{CredSSP="true"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'winrm set winrm/config/listener?Address=*+Transport=HTTP @{Port="5985"}'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'netsh advfirewall firewall set rule group="remote administration" new enable=yes'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'netsh firewall add portopening TCP 5985 "Port 5985"'
    Invoke-ExpressionAndLog -invokeWithCmdExe -command 'net start winrm'
}

function Set-PasswordExpiry { # TODO fixme use pure Powershell
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $accountName,
        [parameter(mandatory=$true)] [bool] $expirePassword
    )
    $passwordExpires = if ($expirePassword) {"TRUE"} else {"FALSE"}
    $command = @"
wmic useraccount where "name='{0}'" set "PasswordExpires={1}"
"@ 
    $command = $command -f $accountName, $passwordExpiress
    Invoke-ExpressionAndLog -invokeWithCmdExe -command $command
}

<#
.synopsis
Set all attached networks to Private
.description
(On some OSes) you cannot enable Windows PowerShell Remoting on network connections that are set to Public
Spin through all the network locations and if they are set to Public, set them to Private
using the INetwork interface:
http://msdn.microsoft.com/en-us/library/windows/desktop/aa370750(v=vs.85).aspx
For more info, see:
http://blogs.msdn.com/b/powershell/archive/2009/04/03/setting-network-location-to-private.aspx
#>
function Set-AllNetworksToPrivate {
    [cmdletbinding()] param()

    # Network location feature was only introduced in Windows Vista - no need to bother with this
    # if the operating system is older than Vista
    if([environment]::OSVersion.version.Major -lt 6) {
        Write-EventLogWrapper "Set-AllNetworksToPrivate: Running on pre-Vista machine, no changes necessary" 
        return 
    }
    Write-EventLogWrapper "Setting all networks to private..."
    
    if(1,3,4,5 -contains (Get-WmiObject win32_computersystem).DomainRole) { throw "Cannot change network location on a domain-joined computer" }
    
    # Disable the GUI which will modally pop up (at least on Win10) lol
    New-Item "HKLM:\System\CurrentControlSet\Control\Network\NewNetworkWindowOff" -force | out-null
    
    # Get network connections
    $networkListManager = [Activator]::CreateInstance([Type]::GetTypeFromCLSID([Guid]"{DCB00C01-570F-4A9B-8D69-199FDBA5723B}"))
    foreach ($connection in $networkListManager.GetNetworkConnections()) {
        $connName = $connection.GetNetwork().GetName()
        $oldCategory = $connection.GetNetwork().GetCategory()
        $connection.getNetwork().SetCategory(1)
        $newCategory = $connection.GetNetwork().GetCategory()
        Write-EventLogWrapper "Changed connection category for '$connName' from '$oldCategory' to '$newCategory'"
    }
}

<#
function Get-PowerScheme {
    [cmdletbinding(DefaultParameterSetName("Active"))] param(
        [Parameter(Mandatory=$true,ParameterSetName="Active")] [switch] $Active,
        [Parameter(Mandatory=$true,ParameterSetName="ByGuid")] [switch] $ByGuid,
        [Parameter(Mandatory=$true,ParameterSetName="ByName")] [switch] $ByName,
    )
    $powerScheme = New-Object PSObject -Property @{Name="";GUID="";}
    $psre = '^Power Scheme GUID\:\s+([A-F0-9]{8}-[A-F0-9]{4}-[A-F0-9]{4}-[A-F0-9]{4}-[A-F0-9]{12})\s+\((.*)\)'
        
    switch ($PsCmdlet.ParameterSetName) {
        "Active" {
            $activeSchemeString = powercfg /getactivescheme
            if ($activeSchemeString -match $psre) {
                $powerScheme.Name = $matches[2]
                $powerScheme.GUID = $matches[1]
            }
            else { write-error "Error: could not find active power configuration"}
        }
        "ByGuid" {
            foreach ($powerSchemeString in (powercfg /list)) {
                $
            }
        }
        "ByName" {}
        default {write-error "Error: not sure how to process a parameter set named $($PsCmdlet.ParameterSetName)"}
    }


    return $powerScheme
}
#>

<#
.synopsis
Set the idle time that must elapse before Windows will power off a display
.parameter seconds 
The number of seconds before poweroff. A value of 0 means never power off.
.notes
AFAIK, this cannot be done without shelling out to powercfg  
#>
function Set-IdleDisplayPoweroffTime {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [int] $seconds
    )
    $currentScheme = (powercfg /getactivescheme).split()[3]
    $DisplaySubgroupGUID = "7516b95f-f776-4464-8c53-06167f40cc99"
    $TurnOffAfterGUID = "3c0bc021-c8a8-4e07-a973-6b14cbcb2b7e"
    set-alias powercfg "${env:SystemRoot}\System32\powercfg.exe"
    powercfg /setacvalueindex $currentScheme $DisplaySubgroupGUID $TurnOffAfterGUID 0 
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
