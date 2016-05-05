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
        $ArchitectureId.i386  = "http://7-zip.org/a/7z920.msi"
        $ArchitectureId.amd64 = "http://7-zip.org/a/7z920-x64.msi"
    }
    UltraDefragDownload = @{
        $ArchitectureId.i386  = "http://downloads.sourceforge.net/project/ultradefrag/stable-release/6.1.0/ultradefrag-portable-6.1.0.bin.i386.zip"
        $ArchitectureId.amd64 = "http://downloads.sourceforge.net/project/ultradefrag/stable-release/6.1.0/ultradefrag-portable-6.1.0.bin.amd64.zip"
    }
    SdeleteDownload = "http://download.sysinternals.com/files/SDelete.zip"
    WindowsIsoDownload = @{
        $WindowsVersionId.w81 = @{
            $ArchitectureId.i386  = @{
                URL  = "http://care.dlservice.microsoft.com/dl/download/B/9/9/B999286E-0A47-406D-8B3D-5B5AD7373A4A/9600.17050.WINBLUE_REFRESH.140317-1640_X86FRE_ENTERPRISE_EVAL_EN-US-IR3_CENA_X86FREE_EN-US_DV9.ISO"
                SHA1 = "4ddd0881779e89d197cb12c684adf47fd5d9e540"
            }
            $ArchitectureId.amd64 = @{
                URL  = "http://download.microsoft.com/download/B/9/9/B999286E-0A47-406D-8B3D-5B5AD7373A4A/9600.16384.WINBLUE_RTM.130821-1623_X64FRE_ENTERPRISE_EVAL_EN-US-IRM_CENA_X64FREE_EN-US_DV5.ISO"
                SHA1 = "5e4ecb86fd8619641f1d58f96e8561ec"
            }
        }
        $WindowsVersionId.w10 = @{
            $ArchitectureId.i386  = @{
                URL  = "http://care.dlservice.microsoft.com/dl/download/C/3/9/C399EEA8-135D-4207-92C9-6AAB3259F6EF/10240.16384.150709-1700.TH1_CLIENTENTERPRISEEVAL_OEMRET_X86FRE_EN-US.ISO"
                SHA1 = "875b450d67e7176b8b3c72a80c60a0628bf1afac"
            }
            $ArchitectureId.amd64 = @{
                URL  = "http://care.dlservice.microsoft.com/dl/download/C/3/9/C399EEA8-135D-4207-92C9-6AAB3259F6EF/10240.16384.150709-1700.TH1_CLIENTENTERPRISEEVAL_OEMRET_X64FRE_EN-US.ISO"
                SHA1 = "56ab095075be28a90bc0b510835280975c6bb2ce"
            }
        }
    }
}
$script:ScriptPath = $MyInvocation.MyCommand.Path
    
### Private support functions I use behind the scenes

<#
.description
Do some very basic filename sanitization
#>
function Get-SanitizedFilename {
    [cmdletbinding()] param(
        [Parameter(Mandatory=$true)] [String] $fileName
    )
    $invalidChars = [System.IO.Path]::GetInvalidFileNameChars()
    $replacementCharacter = "_"
    $newName = [System.String]::Copy($fileName)
    foreach ($invChar in $invalidChars) {
        $newName = $newName.Replace($invChar, $replacementCharacter)
    }
    return $newName
}

<#
.synopsis
Get a rooted path
.notes
Especially useful for .NET functions, which don't understand Powershell's $pwd, and instead have their own concept of the working directory, which is (in any normal case) always %USERPROFILE%. This means that if you do this:

    cd C:\Windows
    (New-Object System.Net.WebClient).DownloadFile("http://example.com/file.txt", "./file.txt")

... the file will be downloaded to %USERPROFILE%\file.txt, not C:\Windows\file.txt
#>
function Get-RootedPath {
    [cmdletbinding()] param(
        [Parameter(Mandatory=$true)] [String] $path
    )
    if (-not [System.IO.Path]::IsPathRooted($path)) {
        $path = Join-Path -Path $pwd -ChildPath $path
    }
    try {
        $rootedPath = [System.IO.Path]::GetFullPath($path)
    }
    catch {
        Write-Error "Failed to validate path '$path'"
        throw $_
    }
    return $rootedPath
}

<#
.synopsis
Download a URL from the web
.parameter url
The URL to download
.parameter outDir
Save the file to this directory. The filename will be the last part of the URL. This will make sense for a basic case like http://example.com/file.txt, but might be a little ugly for URLs like http://example.com/?product=exampleProduct&version=exampleVersion
.parameter outFile
Save the file to this exact filename.
.notes
Why not use Invoke-WebRequest or Invoke-RestMethod? Because those are not available before Powershell 3.0, and I still want to be able to use this function on vanilla Windows 7 (hopefully just before applying all updates and getting a more recent Powershell, but still.)
#>
function Get-WebUrl {
    [cmdletbinding(DefaultParameterSetName="outDir")] param(
        [parameter(mandatory=$true)] [string] $url,
        [parameter(mandatory=$true,ParameterSetName="outDir")] [string] $outDir,
        [parameter(mandatory=$true,ParameterSetName="outFile")] [string] $outFile
    )
    if ($PScmdlet.ParameterSetName -match "outDir") {
        # If the URL is http://example.com/whatever/somefile.txt, the last URL component is somefile.txt
        $lastUrlComponent = [System.IO.Path]::GetFileName($url)
        $filename = Get-SanitizedFilename -fileName $lastUrlComponent
        $outFile = Join-Path -Path $outDir -ChildPath $fileName
    }
    $outFile = Get-RootedPath $outFile
    Write-EventLogWrapper "Downloading '$url' to '$outFile'..."
    (New-Object System.Net.WebClient).DownloadFile($url, $outFile)
    return (Get-Item $outFile)
}

<#
.synopsis
Invoke an expression; log the expression, optionally with any output, and the last exit code if appropriate
#>
function Invoke-ExpressionEx {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $command,
        [switch] $invokeWithCmdExe,
        [switch] $checkExitCode,
        [switch] $logToStdout,
        [int] $sleepSeconds
    )
    $global:LASTEXITCODE = 0
    if ($invokeWithCmdExe) {
        $commandSb = {cmd /c "$command"}.GetNewClosure()
    }
    else {
        $commandSb = {invoke-expression -command $command}.GetNewClosure()
    }
    Write-EventLogWrapper "Invoke-ExpressionEx called to run command '$command'`r`n`r`nUsing scriptblock: $($commandSb.ToString())"
    $output = $null
    
    try {
        if ($logToStdout) { 
            $commandSb.invoke() 
            $message = "Expression '$command' exited with code '$LASTEXITCODE'"
        } 
        else { 
            $output = $commandSb.invoke() 
            $message = "Expression '$command' exited with code '$LASTEXITCODE' and output the following to the console:`r`n`r`n$output"
        }
        Write-EventLogWrapper -message $message 
    }
    catch {
        Write-EventLogWrapper -message "Invoke-ExpressionEx failed to run command '$command'"
        Write-ErrorStackToEventLog -errorStack $_
        throw $_
    }
    
    if ($checkExitCode -and $global:LASTEXITCODE -ne 0) {
        throw "LASTEXITCODE: ${global:LASTEXITCODE} for command: '${command}'"
    }
    if ($sleepSeconds) { start-sleep $sleepSeconds }
}

### Publicly exported functions called directly from slipstreaming scripts

<#
.synopsis
Create a temporary directory
#>
function New-TemporaryDirectory {
    $dirPath = [System.IO.Path]::GetTempFileName() # creates a file automatically
    rm $dirPath
    mkdir $dirPath # mkdir returns a DirectoryInfo object; not capturing it here returns it to the caller
}

<#
.synopsis
Return an object containing metadata for the trial ISO for a particular version of Windows
.notes
TODO: this sucks but I can't think of anything better to do
#>
function Get-WindowsTrialISO {
    [cmdletbinding()] param(
        $WindowsVersion = ([Environment]::OSVersion.Version),
        $WindowsArchitecture = (Get-OSArchitecture)
    )
    if ($WindowsVersion.Major -eq 6 -and $WindowsVersion.Minor -eq 3) {
        return $URLs.WindowsIsoDownload.w81.$WindowsArchitecture
    }
    elseif ($WindowsVersion.Major -eq 10 -and $WindowsVersion.Minor -eq 0) {
        return $URLs.WindowsIsoDownload.w10.$WindowsArchitecture
    }
    else {
        throw "No URL known for Windows version '$WindowsVersion' and architecture '$WindowsArchitecture'"
    }
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
    if ($messagePlus.length -gt 32766) { $messagePlus = $messagePlus.SubString(0,32766) } # Because Write-EventLog will die otherwise
    Write-Host -foreground magenta "====Writing to $EvengLogName event log===="
    Write-Host -foreground darkgray (get-date -Format "yyyy-MM-dd HH:mm:ss")              # The event log tracks the date, but writing to host never shows it
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
        Write-ErrorStackToEventLog -errorStack $error
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

function Test-PowershellSyntax {
    [cmdletbinding(DefaultParameterSetName='FromText')]
    param(
        [parameter(mandatory=$true,ParameterSetName='FromText')] [string] $text,
        [parameter(mandatory=$true,ParameterSetName='FromFile')] [string] $fileName,
        [switch] $ThrowOnFailure
    )
    $tokens = @()
    $parseErrors = @()
    $parser = [System.Management.Automation.Language.Parser]
    if ($pscmdlet.ParameterSetName -eq 'FromText') {
        $parsed = $parser::ParseInput($text, [ref]$tokens, [ref]$parseErrors)
    }
    elseif ($pscmdlet.ParameterSetName -eq 'FromFile') {
        $fileName = resolve-path $fileName
        $parsed = $parser::ParseFile($fileName, [ref]$tokens, [ref]$parseErrors)
    }
    write-verbose "$($tokens.count) tokens found."

    if ($parseErrors.count -gt 0) {
        $message = "$($parseErrors.count) parse errors found in file '$fileName':`r`n"
        $parseErrors |% { $message += "`r`n    $_" }
        if ($ThrowOnFailure) { throw $message } else { write-verbose $message }
        return $false
    }
    return $true
}


<#
.description
Set a scheduled task to run on next logon of the calling user. Intended for tasks that need to reboot and then be restarted such as applying Windows Updates 
.notes
The Powershell New-ScheduledTask cmdlet is broken for me on Win81, but SchTasks.exe doesn't support actions with long arguments (requires a command line of < 200something characters). lmfao. 
My workaround is to take a scriptblock, and then just save it to a file and call the file from Powershell. 
I create the scheduled task with SchTasks.exe, then modify it with Powershell cmdlets that can handle long arguments just fine 
#>
function Set-RestartScheduledTask {
    [cmdletbinding()] param(
        [Parameter(Mandatory=$true)] [Scriptblock] $restartCommand,
        [string] $tempRestartScriptPath = "${env:temp}\$ScriptProductName-TempRestartScript.ps1",
        [string] $taskName = "$ScriptProductName-RestartTask"
    )
    Remove-RestartScheduledTask -taskName $taskName
    
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent().Name
    
    $restartCommand.ToString() | Out-File -FilePath $tempRestartScriptPath
    "Unregister-ScheduledTask -taskName '$taskName' -Confirm:`$false" | Out-File -Append -FilePath $tempRestartScriptPath
    Test-PowershellSyntax -ThrowOnFailure -FileName $tempRestartScriptPath
    
    $schTasksCmd = 'SchTasks.exe /create /sc ONLOGON /tn "{0}" /tr "cmd.exe /c echo TemporparyPlaceholderCommand" /ru "{1}" /it /rl HIGHEST /f' -f $taskName,$currentUser 
    Invoke-ExpressionEx -command $schTasksCmd -invokeWithCmdExe -checkExitCode
        
    # SchTasks.exe cannot specify a user for the LOGON schedule - it applies to all users. Modify it here:
    $trigger = New-ScheduledTaskTrigger -AtLogon -User $currentUser
    # SchTasks.exe cannot specify an action with long arguments (maxes out at like 200something chars). Modify it here: 
    $action = New-ScheduledTaskAction -Execute "$PSHome\Powershell.exe" -Argument "-File `"$tempRestartScriptPath`""
    Set-ScheduledTask -taskname $taskName -action $action -trigger $trigger
    
    $message  = "Created scheduled task called '$taskName', which will run a temp file at '$tempRestartScriptPath', containing:`r`n`r`n"
    $message += (Get-Content $tempRestartScriptPath) -join "`r`n"
    Write-EventLogWrapper -message $message 
}

function Get-RestartScheduledTask {
    [cmdletbinding()] param(
        [string] $taskName = $ScriptProductName
    )
    Get-ScheduledTask |? -Property TaskName -match $taskName
}

function Remove-RestartScheduledTask {
    [cmdletbinding()] param(
        [string] $taskName = $ScriptProductName
    )
    $existingTask = Get-RestartScheduledTask -taskName $taskName 
    if ($existingTask) {
        Write-EventLogWrapper -message "Found existing task named '$taskName'; deleting..."
        Unregister-ScheduledTask -InputObject $existingTask -Confirm:$false | out-null
    }
    else {
        Write-EventLogWrapper -message "Did not find any existing task named '$taskName'"
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
        Invoke-ExpressionEx -checkExitCode -command $msiCall -sleepSeconds 30
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
        Invoke-ExpressionEx -checkExitCode -command ('& "{0}" add-trusted-publisher "{1}" --root "{1}"' -f "$baseDir\cert\VBoxCertUtil.exe",$oracleCert)
        Write-EventLogWrapper "Installing the virtualbox additions"
        Invoke-ExpressionEx -checkExitCode -command ('& "{0}" /with_wddm /S' -f "$baseDir\VBoxWindowsAdditions.exe") # returns IMMEDIATELY, goddamn fuckers
        while (get-process -Name VBoxWindowsAdditions*) { write-host 'Waiting for VBox install to finish...'; sleep 1; }
        Write-EventLogWrapper "virtualbox additions have now been installed"
    }
    
    switch ($PSCmdlet.ParameterSetName) {
        "InstallFromIsoPath" {
            $isoPath = resolve-path $isoPath | select -expand Path
            $vbgaPath = mkdir -force "${env:Temp}\InstallVbox" | select -expand fullname
            try {
                Write-EventLogWrapper "Extracting iso at '$isoPath' to directory at '$vbgaPath'..."
                Invoke-ExpressionEx -checkExitCode -command ('sevenzip x "{0}" -o"{1}"' -f $isoPath, $vbgaPath)
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

function Set-AutoAdminLogon {
    [CmdletBinding(DefaultParameterSetName="Enable")] param(
        [Parameter(Mandatory=$true,ParameterSetName="Enable")] [String] $Username,
        [Parameter(Mandatory=$true,ParameterSetName="Enable")] [String] $Password,
        [Parameter(Mandatory=$true,ParameterSetName="Disable")] [Switch] $Disable
    )
    if ($PsCmdlet.ParameterSetName -Match "Disable") {
        Write-EventLogWrapper "Disabling auto admin logon"
        $AutoAdminLogon = 0
        $Username = ""
        $Password = ""
    }
    elseif ($PsCmdlet.ParameterSetName -Match "Enable") {
        Write-EventLogWrapper "Enabling auto admin logon for user '$Username'"
        $AutoAdminLogon = 1
    }
    else {
        throw "Invalid parameter set name"
    }
    $winLogonKey = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon"
    Set-ItemProperty -Path $winLogonKey -Name "AutoAdminLogon"  -Value $AutoAdminLogon
    Set-ItemProperty -Path $winLogonKey -Name "DefaultUserName" -Value $Username
    Set-ItemProperty -Path $winLogonKey -Name "DefaultPassword" -Value $Password
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
    
    $ngen32path = "${env:WinDir}\microsoft.net\framework\v4.0.30319\ngen.exe"
    # Invoke-ExpressionEx "$ngen32path update /force /queue"
    # Invoke-ExpressionEx "$ngen32path executequeueditems"
    set-alias ngen32 $ngen32path
    ngen32 update /force /queue
    ngen32 executequeueditems
        
    if ((Get-OSArchitecture) -match $ArchitectureId.amd64) { 
        $ngen64path = "${env:WinDir}\microsoft.net\framework64\v4.0.30319\ngen.exe"
        # Invoke-ExpressionEx "$ngen64path update /force /queue"
        # Invoke-ExpressionEx "$ngen64path executequeueditems"
        set-alias ngen64 $ngen64path
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
        Invoke-ExpressionEx -command ('sevenzip x "{0}" "-o{1}"' -f $udfZipPath,$env:temp)

        $sdZipPath = Get-WebUrl -url $URLs.SdeleteDownload -outDir $env:temp
        $sdExPath = "${env:temp}\SDelete"
        # This archive does NOT contain a folder - extract it to a subfolder (will create if necessary)
        Invoke-ExpressionEx -command ('sevenzip x "{0}" "-o{1}"' -f $sdZipPath,$sdExPath)

        stop-service wuauserv
        rm -recurse -force ${env:WinDir}\SoftwareDistribution\Download
        start-service wuauserv

        Invoke-ExpressionEx -logToStdout -command ('& {0} --optimize --repeat "{1}"' -f "$udfExPath\udefrag.exe","$env:SystemDrive")
        Invoke-ExpressionEx -command ('& {0} /accepteula -q -z "{1}"' -f "$sdExPath\SDelete.exe",$env:SystemDrive)
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

<#
.notes
One nice thing about FF and Chrome is that you don't have to handle updates yourself - they both install services that update the browser for you
#>
function Install-Firefox {
    [cmdletbinding()] param(
        [ValidateSet("Standard","ESR")] [String] $edition = "Standard",
        [String] $language = "en-US"
    )
    switch ($edition) {
        "Standard" {$downloadPageUrl = 'https://www.mozilla.org/en-US/firefox/all/'}
        "ESR"      {$downloadPageUrl = 'https://www.mozilla.org/en-US/firefox/organizations/all'}
    }
    $osarch = Get-OSArchitecture
    switch ($osarch) {
        $ArchitectureId.amd64 {$os = 'win64'}
        $ArchitectureId.i386 {$os = 'win'}
    }
    $firefoxIniFile = "${env:temp}\firefox-installer.ini"

    try {
        Write-EventLogWrapper "Finding download location for $edition edition of Firefox..."
        $response = Invoke-WebRequest -Uri $downloadPageUrl
        $downloadUrl = $response.ParsedHtml.getElementById($language).getElementsByClassName('download win')[0].getElementsByTagName('a') | Select -Expand href
        $firefoxInstallerFile = Get-WebUrl -url $downloadUrl -outFile "${env:temp}\firefox-installer.exe"

        $firefoxIniContents = @(
            "QuickLaunchShortcut=false"
            "DesktopShortcut=false"
        )
        Out-File -FilePath $firefoxIniFile -InputObject $firefoxIniContents -Encoding UTF8
        Write-EventLogWrapper "Beginning Firefox installation process..."
        $process = Start-Process -FilePath $firefoxInstallerFile.FullName -ArgumentList @("/INI=`"$firefoxIniFile`"") -Wait
        if ($process.ExitCode -ne 0) {
            throw "Firefox installer at $($firefoxInstallerFile.FullName) exited with code $($process.ExitCode)"
        }
    }
    catch {
        @($firefoxInstallerFile,$firefoxIniFile) |% { if ($_ -and (Test-Path $_)) { Remove-Item $_ } }
        throw $_
    }

    Write-EventLogWrapper "Firefox installation process complete"
    Remove-Item @($firefoxInstallerFile,$firefoxIniFile)
}

function Set-FirefoxOptions {
    [cmdletbinding()] param(
        [switch] $setDefaultBrowser
    )
    @($env:ProgramFiles,${env:ProgramFiles(x86)}) |% {
        $testPath = "$_\Mozilla Firefox\firefox.exe"
        if (Test-Path $testPath) {$ffPath = $testPath} 
    }
    if (-not $ffPath) { throw "Could not find the Firefox install location." }
    if ($setDefaultBrowser) { 
        Start-Process -FilePath $ffPath -ArgumentList @("-silent", "-setDefaultBrowser") -Wait -Verb RunAs
        # This didn't appear to work:
        # $defaultBrowserPath = "HKCU:\Software\Classes\http\shell\open\command"
        # $defaultBrowserValue = '"{0}" -osint -url "%1"' -f $ffPath
        # Set-ItemProperty -path $defaultBrowserPath -name "(default)" -value $defaultBrowserValue
    }
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
    Invoke-ExpressionEx -invokeWithCmdExe -command 'net stop winrm'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'sc.exe config winrm start= auto'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm quickconfig -q'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm quickconfig -transport:http'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config @{MaxTimeoutms="1800000"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/winrs @{MaxMemoryPerShellMB="2048"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/service @{AllowUnencrypted="true"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/client @{AllowUnencrypted="true"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/service/auth @{Basic="true"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/client/auth @{Basic="true"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/service/auth @{CredSSP="true"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'winrm set winrm/config/listener?Address=*+Transport=HTTP @{Port="5985"}'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'netsh advfirewall firewall set rule group="remote administration" new enable=yes'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'netsh firewall add portopening TCP 5985 "Port 5985"'
    Invoke-ExpressionEx -invokeWithCmdExe -command 'net start winrm'
}

function Add-LocalSamUser {
    [cmdletbinding()] param(
        [Parameter(Mandatory=$true)] [string] $userName,
        [Parameter(Mandatory=$true)] [string] $password,
        [string] $fullName,
        [switch] $PassThru
    )
    Write-EventLogWrapper "Creating a new local user called '$userName'"
    $computer = [ADSI]"WinNT://$env:COMPUTERNAME,Computer"
    $newUser = $computer.Create("User", $userName)
    $newUser.SetPassword($password)
    $newUser.SetInfo()
    $newUser.FullName = $fullName 
    $newUser.SetInfo()
    Add-LocalSamUserToGroup -userName $userName -groupName "Users"
    if ($PassThru) { return $newUser }
}

function Add-LocalSamUserToGroup {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $userName,
        [parameter(mandatory=$true)] [string] $groupName
    )
    Write-EventLogWrapper "Adding '$userName' to the local '$groupName' group"
    $localAdmins = [ADSI]"WinNT://$env:COMPUTERNAME/$groupName,group"
    $localAdmins.Add("WinNT://$userName")
}

function Set-PasswordExpiry { # TODO fixme use pure Powershell
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $accountName,
        [parameter(mandatory=$true,ParameterSetName="EnablePasswordExpiry")] [switch] $enable,
        [parameter(mandatory=$true,ParameterSetName="DisablePasswordExpiry")] [switch] $disable
    )
    $passwordExpires = if ($PsCmdlet.ParameterSetName -match "EnablePasswordExpiry") {"TRUE"} else {"FALSE"}
    $command = @"
wmic useraccount where "name='{0}'" set "PasswordExpires={1}"
"@ 
    $command = $command -f $accountName,$passwordExpires
    Invoke-ExpressionEx -command $command
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
