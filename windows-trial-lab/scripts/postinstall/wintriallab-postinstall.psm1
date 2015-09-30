<# 
jesus fucking christ
fucking Packer
#>

<#
.notes
This is intended for use in the postinstall phase. 
Only functions that were intended to run in that phase should have the concept of a "LabTempDir".
TODO: make sure this is always a 100% normalized path
#>
function Get-LabTempDir {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    if ("${script:WinTrialLabTemp}") {} # noop
    elseif ("${env:WinTrialLabTemp}") {
        $script:WinTrialLabTemp = $env:WinTrialLabTemp
    }
    else {
        $dateStamp = get-date -UFormat "%Y-%m-%d-%H-%M-%S"
        $script:WinTrialLabTemp = "${env:Temp}\WinTrialLab-$dateStamp" 
    }

    if (-not (test-path $script:WinTrialLabTemp)) {
        write-verbose "Temporary directory does not exist, creating it..."
        mkdir -force $script:WinTrialLabTemp | out-null
    }
    $script:WinTrialLabTemp = resolve-path $script:WinTrialLabTemp | select -expand Path
    write-verbose "Using WinTrialLabTemp directory at '${script:WinTrialLabTemp}'"
    return $script:WinTrialLabTemp
}

function Invoke-ExpressionAndCheck {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $command
    )
    $global:LASTEXITCODE = 0
    write-verbose "Invoking expression '$command'"
    invoke-expression -command $command
    write-verbose "Expression '$command' had a last exit code of '$LastExitCode'"
    if ($global:LASTEXITCODE -ne 0) {
        throw "LASTEXITCODE: ${global:LASTEXITCODE} for command: '${command}'"
    }
}

# function Copy-ItemAndExclude {
#     [cmdletbinding()] param(
#         [parameter(mandatory=$true)] [string] $path,
#         [parameter(mandatory=$true)] [string] $destination,
#         [parameter(mandatory=$true)] [string[]] $exclude,
#         [switch] $force
#     )
#     $path = resolve-path $path | select -expand path
#     $sourceItems = Get-ChildItem -Path $path -Recurse -Exclude $exclude 
#     write-verbose "Found $($sourceItems.count) items to copy from '$path'"
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

function Get-WebUrl {
    param(
        [parameter(mandatory=$true)] [string] $url,
        [parameter(mandatory=$true)] [string] $downloadPath
    )
    write-verbose "Function: $($MyInvocation.MyCommand)..."

    if (test-path $downloadPath) {
        if ((get-item $downloadPath).gettype().name -match "DirectoryInfo") {
            throw "Must provide a full path, including filename"
        }
        else {
            $downloadPath = resolve-path $downloadPath | select -expand path
        }
    }
    else {
        $downloadParent = split-path $downloadPath -parent
        $downloadFilename = split-path $downloadPath -leaf
        if (-not "$downloadParent") { 
            $downloadParent = $pwd | select -expand Path
        }
        elseif (-not (test-path $downloadParent)) {
            mkdir -force $downloadParent | out-null
        }
        $downloadParent = resolve-path $downloadParent 
        $downloadPath = "$downloadParent\$downloadFilename" -replace ""
    }
    write-verbose "Downloading url '$url' to path '$downloadPath'"
    (New-Object System.Net.WebClient).DownloadFile($url, $downloadPath)
}

$ArchitectureId = @{
    i386 = "i386"
    amd64 = "amd64"
}

<#
.description
Return the OS Architecture, as determined by WMI
Will return either "i386" or "amd64"
TODO: this isn't a great method but I'm tired of trying to find the totally correct one. This one isn't ideal because OSArchitecture can be localized. 
Supposedly the canonical way is calling into the registry: 
- http://stackoverflow.com/a/24590583/868206
- https://support.microsoft.com/en-us/kb/556009
#>
function Get-OSArchitecture {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    #reg Query "HKLM\Hardware\Description\System\CentralProcessor\0" | find /i "x86" > NUL && set OSARCHITECTURE=32BIT || set OSARCHITECTURE=64BIT
    $OSArch = Get-WmiObject -class win32_operatingsystem -property osarchitecture | select -expand OSArchitecture

    if ($OSArch -match "64") { return $ArchitectureId.amd64 } 
    elseif ($OSArch -match "32") { return $ArchitectureId.i386 }
    else { throw "Could not determine OS Architecture from string '$OSArch'" }
}

function Test-AdminPrivileges {
    $me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    return $me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
}

function Limit-TextWidth {
    [cmdletbinding()] 
    param(
        [parameter(mandatory=$true)] [string] $text,
        [parameter(mandatory=$true)] [int] $width,
        [int] $indentSpaces = 0
    )
    $width = $width -1
    if ($indentSpaces -ge $width) {
        throw "`$indentSpaces must be smaller than `$width"
    }
    $indent = " " * $indentSpaces
    $output = ""
    $ctr=0
    foreach ($line in ($text -split "`n")) {
        $ctr+=1
        #write-host -foreground cyan "${ctr}: $line"

        $finished = $false
        while (-not $finished) {
            $line = "$indent$line"
            if ($line.length -gt $width) {
                $output += $line.substring(0,$width)
                $output += "`n"
                $line = $line.substring($width)
            }
            else {
                $output += $line
                $output += "`n"
                $finished = $true
            }
        }

        if ($output[-1] -ne "`n") {
            $output += "`n"
        }
    }
    return $output
}

function Show-ErrorReport {
    [cmdletbinding()]
    param(
        [switch] $ExitIfErrors
    )
    write-verbose "`$error.count = $($error.count)"
    write-verbose "`$LASTEXITCODE = $LastExitCode"    

    if ($Host -and $Host.UI -and $Host.UI.RawUI) {
        $wrapWidth = $Host.UI.RawUI.Buffersize.Width
    }
    else {
        $wrapWidth = 9999
    }

    $doExit = $false
    $reportString = "ERROR Report: No errors`n"

    if ($error.count -or $LASTEXITCODE) {
        $errorSummary = "`$LASTEXITCODE=$LastExitCode, `$Error.count=$($Error.count)"
        $reportString = "ERROR Report: $errorSummary`n`n"

        for ($i= $error.count -1; $i -ge 0; $i -= 1) {
            write-verbose "Processing error $i"
            $e = $error[$i]

            $errorDetails  = "PS `$Error[$i]: `n"
            $indentCount = 4

            # $error can contain at least 2 kinda of objects - ErrorRecord objects, and things that wrap ErrorRecord objects
            # The information we need is found in the ErrorRecord objects, so unwrap them here if necessary
            if ($e.ErrorRecord) {
                $e = $e.ErrorRecord
            }

            $errorDetails += Limit-TextWidth -text $e.ToString() -width $wrapWidth -indent $indentCount
            if ($errorDetails[-1] -ne "`n") { $errorDetails += "`n" }

            if ($e.ScriptStackTrace) {
                $errorDetails += Limit-TextWidth -text $e.ScriptStackTrace -width $wrapWidth -indent $indentCount
                if ($errorDetails[-1] -ne "`n") { $errorDetails += "`n" }
            }

            $reportString += $errorDetails
        }

        if ($ExitIfErrors) { 
            $doExit = $true
            $reportString += "Exiting with returncode 1...`n" 
        }
    }

    write-output "----`n$reportString----"
    if ($doExit) { 
        exit 1
    }
}

$script:szInstallDir = "$env:ProgramFiles\7-Zip"
set-alias sevenzip "${script:szInstallDir}\7z.exe"
function Install-SevenZip {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    $OSArch = Get-OSArchitecture
    if ($OSArch -match "i386") { 
        $szFilename = "7z920.msi" 
    }
    elseif ($OSArch -match "amd64") { 
        $szFilename = "7z920-x64.msi" 
    }
    else { 
        throw "Cannot install 7-zip for an architecture of '$OSArch'" 
    }

    $szUrl = "http://7-zip.org/a/$szFilename"
    $szDlPath = "$(Get-LabTempDir)\$szFilename"
    Get-WebUrl -url $szUrl -downloadPath $szDlPath

    write-verbose "Downloaded '$szUrl' to '$szDlPath', now running msiexec..."    

    msiexec /qn /i "$szDlPath"
    sleep 30 # Windows is bad, written by bad people who write bad software. More like softWHEREdidyougetthisideaitSUCKS amirite??
    if ($LASTEXITCODE -and ($LASTEXITCODE -ne 0)) { throw "External command failed with code '$LASTEXITCODE'" }

    $szExePath = get-item (gcm sevenzip | select -expand definition)
    write-verbose "Installed 7-zip to: $szExePath"
}

function Install-VBoxAdditions {
    [cmdletbinding()]
    param(
        [parameter(ParameterSetName="FromIso",mandatory=$true)] [string] $isoPath,
        [parameter(ParameterSetName="FromDisc",mandatory=$true)] [switch] $fromDisc
    )

    if ($PsCmdlet.ParameterSetName -match "FromIso") {
        write-verbose "Function: $($MyInvocation.MyCommand)..."
        $isoPath = resolve-path $isoPath | select -expand Path
        $vbgaPath = "$(Get-LabTempDir)\InstallVbox"
        mkdir -force $vbgaPath

        write-verbose "Extracting iso at '$isoPath' to directory at '$vbgaPath'..."
        sevenzip x "$isoPath" "-o$vbgaPath"
        if ($LASTEXITCODE -and ($LASTEXITCODE -ne 0)) { throw "External command failed with code '$LASTEXITCODE'" }
    }
    elseif ($PsCmdlet.ParameterSetName -match "FromDisc") {
        foreach ($drive in (Get-PSDrive -PSProvider FileSystem)) {
            if (test-path "$($drive.Name):\VboxWindowsAdditions.exe") {
                $vbgaPath = "$($drive.Name):"
                break
            }
        }
    }
    else {
        throw "No such parameter set '$($psCmdlet.ParameterSetName)'"
    }
    if (-not "$vbgaPath") { throw "Could not find VBox Guest Additions" }

    write-verbose "Installing the Oracle certificate..."
    set-alias VboxCertUtil "$vbgaPath\cert\VBoxCertUtil.exe"
    $oracleCert = resolve-path "$vbgaPath\cert\oracle-vbox.cer" | select -expand path
    VboxCertUtil add-trusted-publisher $oracleCert --root $oracleCert
    # NOTE: Checking for exit code, but this command will fail with an error if the cert is already installed
    # TODO: what is the error code in that case? get it and ignore it here 
    if ($LASTEXITCODE -and ($LASTEXITCODE -ne 0)) { throw "External command failed with code '$LASTEXITCODE'" }

    write-verbose "Installing the virtualbox additions"
    set-alias VBoxWindowsAdditions "$vbgaPath\VBoxWindowsAdditions.exe"
    $startTime = get-date
    VBoxWindowsAdditions /with_wddm /S # returns IMMEDIATELY
    while (get-process -Name VBoxWindowsAdditions*) { write-verbose 'Waiting for VBox install to finish...'; sleep 1; }
    $endTime = get-date
    write-verbose "Running the VboxWindowsAdditions installer took $($endTime - $startTime | select -expand seconds) seconds"
}

function Disable-AutoAdminLogon {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    set-itemproperty -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon" -Name AutoAdminLogon -Value 0    
}

function Enable-RDP {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    netsh advfirewall firewall add rule name="Open Port 3389" dir=in action=allow protocol=TCP localport=3389
    reg add "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Terminal Server" /v fDenyTSConnections /t REG_DWORD /d 0 /f
}

# TODO: only works on x86
function Install-CompiledDotNetAssemblies {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    # http://support.microsoft.com/kb/2570538
    # http://robrelyea.wordpress.com/2007/07/13/may-be-helpful-ngen-exe-executequeueditems/

    set-alias ngen32 "${env:WinDir}\microsoft.net\framework\v4.0.30319\ngen.exe"
    ngen32 update /force /queue
    ngen32 executequeueditems

    if ((Get-OSArchitecture) -match "amd64") { 
        set-alias ngen64 "${env:WinDir}\microsoft.net\framework64\v4.0.30319\ngen.exe"
        ngen64 update /force /queue
        ngen64 executequeueditems
    }
}

function Compress-WindowsInstall {
    write-verbose "Function: $($MyInvocation.MyCommand)..."
    $temp = Get-LabTempDir

    $OSArch = Get-OSArchitecture
    if ($OSArch -match "i386") { $udfArch = "i386" }
    elseif ($OSArch -match "amd64") { $udfArch = "amd64" }
    else {throw "Cannot compact for architecture '$OSArch'"}

    $udfFilename = "ultradefrag-portable-6.1.0.bin.$udfArch.zip"
    $udfUrl = "http://downloads.sourceforge.net/project/ultradefrag/stable-release/6.1.0/$udfFilename"
    $udfZipPath = "$temp\$udfFilename"
    Get-WebUrl -url $udfUrl -downloadPath $udfZipPath
    sevenzip x "$udfZipPath" "-o$temp"
    set-alias udefrag "$temp\ultradefrag-portable-6.1.0.$udfArch\udefrag.exe"

    $sdZipPath = "$temp\SDelete.zip"
    Get-WebUrl -url http://download.sysinternals.com/files/SDelete.zip -downloadPath $sdZipPath
    sevenzip x "$sdZipPath" "-o$temp"
    set-alias sdelete "$temp\SDelete.exe"

    stop-service wuauserv
    rm -recurse -force ${env:WinDir}\SoftwareDistribution\Download
    start-service wuauserv

    udefrag --optimize --repeat "$env:SystemDrive"

    reg.exe ADD HKCU\Software\Sysinternals\SDelete /v EulaAccepted /t REG_DWORD /d 1 /f
    sdelete -q -z "$env:SystemDrive"
}

function Disable-WindowsUpdates {
    if (-not (Test-AdminPrivileges)) {
        throw "Cannot run without administrator privileges"
    }

    $Updates = (New-Object -ComObject "Microsoft.Update.AutoUpdate").Settings
    if ($Updates.ReadOnly) { 
        throw "Cannot update Windows Update settings due to GPO restrictions." 
    }

    $Updates.NotificationLevel = 1 #Disabled
    $Updates.Save()
    $Updates.Refresh()
}

function Enable-MicrosoftUpdate {
    [cmdletbinding()] param()
    write-verbose "Enabling Microsoft Update..."
    stop-service wuauserv
    $auKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update" 
    Set-ItemProperty -path $auKey -name EnableFeaturedSoftware -value 1 
    Set-ItemProperty -path $auKey -name IncludeRecommendedUpdates -value 1 

    $ServiceManager = New-Object -ComObject "Microsoft.Update.ServiceManager"
    $ServiceManager.AddService2("7971f918-a847-4430-9279-4a52d1efe18d",7,"")

    start-service wuauserv
}

function Install-Chocolatey {
    $chocoExePath = "${env:ProgramData}\Chocolatey\bin"

    if ($($env:Path).ToLower().Contains($($chocoExePath).ToLower())) {
        write-verbose "Chocolatey already in path, exiting..."
        return
    }

    # Add to system PATH
    $systemPath = [Environment]::GetEnvironmentVariable('Path', [System.EnvironmentVariableTarget]::Machine)
    $systemPath += ";$chocoExePath"
    [Environment]::SetEnvironmentVariable("PATH", $systemPath, [System.EnvironmentVariableTarget]::Machine)

    # Update local process' path
    $env:Path = $systemPath
    $userPath = [Environment]::GetEnvironmentVariable('Path', [System.EnvironmentVariableTarget]::User)
    if ($userPath) { $env:Path += ";$userPath" }

    # Run the installer
    iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
}

<#
.description
Formerly: 
- reg.exe ADD HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\ /v HideFileExt /t REG_DWORD /d 0 /f
- reg.exe ADD HKCU\Console /v QuickEdit /t REG_DWORD /d 1 /f
#>
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
    write-verbose "Removing Hibernation file..."
    $powerKey = 'HKLM:\SYSTEM\CurrentControlSet\Control\Power'
    Set-ItemProperty -path $powerKey -name HibernateFileSizePercent -value 0  # hiberfil is zero bytes
    Set-ItemProperty -path $powerKey -name HibernateEnabled -value 0          # disable hibernation altogether
}

function Enable-WinRM {
    [cmdletbinding()] param()
    write-verbose "Enabling WinRM..."

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

function Set-PasswordExpiry {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $accountName,
        [parameter(mandatory=$true)] [bool] $expirePassword
    )
    $pe = "TRUE"
    if (-not $expirePassword) { $pe = "FALSE" }
    cmd.exe /c wmic useraccount where "name='$accountName'" set "PasswordExpires=$pe"
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
    write-verbose "Found the WAIK at '$adkPath'"

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

function New-WindowsInstallMedia {
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
    write-verbose "Calling OSCDIMG: '$oscdimgCall"
    Invoke-ExpressionAndCheck $oscdimgCall -verbose:$verbose

    dismount-diskimage -imagepath $sourceIsoPath
}

<#
.notes 
For use with WSUS Offline Updater
#>
function Get-WOShortCode {
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

    write-verbose "Found shortcode '$shortcode' for OS named '$OSName' of architecture '$OSArchitecture'"
    return $shortCode
}


### TEMP SECTION
# This section contains stuff that's probably only useful in development

function Get-DownloadedUpdates {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] [string] $wsusOfflineClientDir
    )
    $updateFiles = ls $wsusOfflineClientDir\w*\*\*kb*
    #$updateFiles = ls $wsusOfflineClientDir\w63\glb\*kb* | select -first 20
    $updates = @()
    foreach ($u in $updateFiles) {
        $update = New-Object PSObject -Property @{ Item = $u }
        if ($u.name -match ".*(kb[0-9]+(\-v[0-9]+)?).*") {
            $kb = $matches[1]
            Add-Member -inputObject $update -NotePropertyMembers @{KB=$kb}
        }
        $updates += @($update)
    }
    return $updates
}
function SearchUpdatesList {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] $updateList,
        [parameter(mandatory=$true)] [string] $kbFragment
    )
    return $updateList |? { $_.kb -match $kbFragment }
}

# Exports:
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
