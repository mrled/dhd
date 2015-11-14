<#
.synopsis
Run Windows Update, installing all available updates and rebooting as necessary
.parameter MaxCycles
The number of times to check for updates before forcing a reboot, even if Windows Update has not indicated that one is required 
TODO: this doesn't appear to be working reliably
.parameter PostUpdateExpression
A string representing a PowerShell expression that is run one time after all updates have been applied (or MaxCycles has been reached without rebooting)
TODO: the parenthetical is nonobvious behavior and should be eliminated. In fact the whole of MaxCycles should be rethought. 
.parameter NoRestart
Don't reboot (useful for debugging)
.notes
This script can be run directly, but it can also be dot-sourced to get access to internal functions without automatically checking for updates, applying them, or rebooting
#>
param(
    [int] $MaxCycles = 5,
    [string] $PostUpdateExpression,
    [switch] $NoRestart
)

<# Notes on the code: 

I'm trying to use StrictMode. That ends up making some code more complex than it would have to be otherwise. For instance, sometimes I have to check that a property exists (like $SomeVar.PSObject.Properties['PropertyName']) before I can use it. 

TODO: Make sure every path of my program is writing unique log messages. Make sure all those paths are working.
TODO: Pass unique event IDs for every event to Write-EventLogWrapper. ??
#>

$ErrorActionPreference = "Stop"

import-module $PSScriptRoot\wintriallab-postinstall.psm1

# I need to get the path to this script from inside functions, which mess with the $MyInvocation variable
$script:ScriptPath = $MyInvocation.MyCommand.Path

function Restart-ComputerAndUpdater {
    param(
        [parameter(mandatory=$true)] [int] $CyclesRemaining,
        [parameter(mandatory=$true)] [switch] $NoRestart
    )
    $restartCommandComponents = @(
        ('& "{0}"' -f $scriptPath)
        "-MaxCycles $CyclesRemaining"
    )
    if ($PostUpdateExpression) { $restartCommandComponents += '-PostUpdateExpression "{0}"' -f "$PostUpdateExpression" }
    if ($NoRestart) { $restartCommandComponents += "-NoRestart" }
    $restartCommand = [ScriptBlock]::Create(($restartCommandComponents -join " "))
    Set-RestartScheduledTask -RestartCommand $restartCommand | out-null
    
    if ($NoRestart) { 
        Write-EventLogWrapper "Restart-ComputerAndUpdater was called, but '-NoRestart' was passed; exiting instead."
        exit 1
    }
    else {
        Write-EventLogWrapper "Restarting..."
        Restart-Computer -Force
        exit 0 # Restarting returns immediately and script execution continues until the reboot is processed. Lol.
    } 
}

<#
.synopsis
Return a new Microsoft Update Session for use with Check-WindowsUpdates and Install-WindowsUpdates
#>
function New-UpdateSession {
    [cmdletbinding()] param()
    $UpdateSession = New-Object -ComObject 'Microsoft.Update.Session'
    $UpdateSession.ClientApplicationID = "win-updates.ps1"
    return $UpdateSession
}

<#
.synopsis
Run the expression passed to this script with -PostUpdateExpression, if any
#>
function Run-PostUpdate {
    if ($PostUpdateExpression) {
        Write-EventLogWrapper -message "Running PostUpdate expression:`r`n`r`n${PostUpdateExpression}"
        Invoke-Expression $PostUpdateExpression
    }
    else {
        Write-EventLogWrapper -message "No PostUpdate to run"
    }
}

<#
.synopsis
Determine whether the machine needs a Windows Update -related reboot
.notes 
Originally from <https://gallery.technet.microsoft.com/scriptcenter/Get-PendingReboot-Query-bdb79542>, but note that this doesn't check all those things. We don't care here whether the machine needs a reboot from joining a domain, for example; we only care about Windows Update.
TODO: apparently this can NOT currently detect reboots that are necessary because someone ran Windows Update manually without rebooting. Fix this.
#>
function Get-RestartPendingStatus {
    [cmdletbinding()] param()
    
    # Component Based Servicing (aka Windows components) (Vista+ only)
    $CsbProp = Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Component Based Servicing"
    if ($CsbProp.PSObject.Properties['RebootPending']) {
        Write-EventLogWrapper "(Un)installation of a Windows component requires a restart" 
        return $true 
    }

    $WuauProp = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update"
    if ($WuauProp.PSObject.Properties['RebootRequired']) {
        Write-EventLogWrapper "Updates have been installed recently, but the machine must be restarted before installation is complete" 
        return $true 
    }
    
    Write-EventLogWrapper "There are no Windows Update -related reboots pending"
    return $false
}

<#
.Description
Install Windows Updates. 
.parameter UpdateSession 
Must be a session created with New-UpdateSession - that is, a COM object of type Microsoft.Update.Session
.parameter UpdateList
A list of updates. Might be in a Microsoft.Update.UpdateColl COM object (possibly from Check-WindowsUpdates), or might be just a normal array of them
.parameter MaxDownloadAttempts
The maximum number of times to attempt to download updates
.outputs
Returns a boolean representing whether a reboot is required
#>
function Install-WindowsUpdates {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] $UpdateSession,
        [parameter(mandatory=$true)] $UpdateList,
        [int] $MaxDownloadAttempts = 12
    )
    
    if (-not $UpdateList) {
        Write-EventLogWrapper -message "No Updates To Download..."
        return $false
    }
    
    # Powershell does something weird to COM objects when passed between 
    # functions (maybe a scoping issue?) so even though $UpdateList may already
    # be an UpdateColl object (as you'd think would be the case if you got it 
    # from Check-WindowsUpdates), we have to create a new one here
    $UpdateComObject = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    $UpdateList |% { $UpdateComObject.Add($_) }
    
    $AcceptedEulas = @()
    foreach ($update in $UpdateComObject) { 
        if ($update.PSObject.Properties['EulaAccepted']) { 
            $AcceptedEulas += ($update)
            $update.AcceptEula()
        }
    }
    $message = "There were $($AcceptedEulas.count) updates with a EULA which was automatically accepted`r`n"
    $AcceptedEulas |% { $message += "`r`n -  $($_.Title)"}
    Write-EventLogWrapper $message
    
    Write-EventLogWrapper -message 'Downloading Updates...'
    $ok = $false
    $attempts = 0
    while ((! $ok) -and ($attempts -lt $MaxDownloadAttempts)) {
        try {
            $Downloader = $UpdateSession.CreateUpdateDownloader()
            $Downloader.Updates = $UpdateComObject
            $Downloader.Download()
            $ok = $true
        } 
        catch {
            $message = "Error downloading updates. Retrying in 30s.`r`n`r`n$($_.Exception)"
            Write-EventLogWrapper -message $message
            $attempts += 1
            Start-Sleep -s 30
        }
    }

    $UpdatesToInstall = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    $message = 'The following updates are downloaded and ready to be installed:'
    foreach ($Update in $UpdateComObject) {
        if ($Update.IsDownloaded) {
            $message += "`r`n -  $($Update.Title)"
            $UpdatesToInstall.Add($Update) |Out-Null
        }
    }
    Write-EventLogWrapper -message $message

    $RebootRequired = $false
    if ($UpdatesToInstall.Count -gt 0) {
        $Installer = $UpdateSession.CreateUpdateInstaller()
        $Installer.Updates = $UpdatesToInstall
        
        if ($Installer.PSObject.Properties['RebootRequiredBeforeInstallation'] -and $Installer.RebootRequiredBeforeInstallation) {
            Write-EventLogWrapper "Reboot required before installation. (This can happen when updates are installed but the machine is not rebooted before trying to install again.)"
            return $true
        }

        $InstallationResult = $Installer.Install()
        $RebootRequired = [bool]$InstallationResult.RebootRequired
        $ResultCode = if ($InstallationResult.PSObject.Properties['ResultCode']) { $InstallationResult.ResultCode } else { $null }
        
        $message =  "Windows Update complete. Installation Result: $ResultCode.`r`nReboot Required: $RebootRequired"
        foreach ($update in $UpdatesToInstall) {
            $message += "`r`n`r`nItem: $($update.Title)"
            $message += "`r`nResult: $ResultCode"
        }
        Write-EventLogWrapper -message $message
    }
    else {
        Write-EventLogWrapper -message 'No updates available to install...'
    }
    return $RebootRequired
}

<#
.Description
Check for Windows Updates and return a list of any updates that need to be applied. 
.parameter UpdateSession 
Must be a session created with New-UpdateSession - that is, a COM object of type Microsoft.Update.Session
.parameter MaxSearchAttempts
The maximum number of times to attempt to search for outstanding updates
.outputs
If there applicable updates, return a Microsoft.Update.UpdateColl COM object; if not, return $null
.notes
After the updates are applied, this should run again because it may detect new updates.
#>
function Check-WindowsUpdates {
    param(
        [parameter(mandatory=$true)] $UpdateSession,
        [switch] $FilterInteractiveUpdates,
        [int] $MaxSearchAttempts = 12
    )
    Write-EventLogWrapper -message "Checking for Windows Updates at $(Get-Date)" -eventId 104
    $SearchResult = $null
    
    $UpdateSearcher = $UpdateSession.CreateUpdateSearcher()
    $successful = $false
    $attempts = 0
    while(-not $successful -and $attempts -lt $MaxSearchAttempts) {
        try {
            $SearchResult = $UpdateSearcher.Search("IsInstalled=0 and Type='Software' and IsHidden=0")
            $successful = $true
        } 
        catch {
            $message = "Search call to UpdateSearcher was unsuccessful. Retrying in 10s.`r`n`r`n$($_.Exception | Format-List -force)"
            Write-EventLogWrapper -message $message
            $attempts += 1
            Start-Sleep -s 10
        }
    }
    if (-not $successful) {
        throw "Unable to retrieve list of outstanding updates"
    }
    
    $ApplicableUpdates = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    $SkippedUpdates = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    foreach ($update in $SearchResult.Updates) {
        if ($FilterInteractiveUpdates -and 
            [bool]$Update.InstallationBehavior.PSObject.Properties['CanRequestUserInput'] -and 
            $Update.InstallationBehavior.CanRequestUserInput) 
        {
            $SkippedUpdates.Add($Update) | out-null
        }
        else { 
            $ApplicableUpdates.Add($Update) | out-null 
        }
    }
    
    $message = "There are $($ApplicableUpdates.Count) outstanding applicable updates"
    if ($ApplicableUpdates.Count -gt 0) {
        $ApplicableUpdates |% { $message += "`r`n -  $($_.Title)" } 
    }
    $message += "`r`n`r`nThere are $($SkippedUpdates.Count) skipped updates"
    if ($SkippedUpdates.Count -gt 0) {
        $SkippedUpdates |% { $message += "`r`n -  $($_.Title)" } 
    }
    Write-EventLogWrapper -message $message
    
    return $ApplicableUpdates
}

<#
.synopsis
Run Windows Update, installing all available updates and rebooting as necessary
.notes
- Skips any update that requires user interaction 
- Automatically accepts the EULA of all the updates it installs 
#>
function Run-WindowsUpdate {
    [cmdletbinding()] param()
    
    $message = "Running Windows Update..."
    Write-EventLogWrapper $message
    
    if (Get-RestartPendingStatus) {
        Restart-ComputerAndUpdater -CyclesRemaining $maxCycles -NoRestart:$NoRestart 
    }
    
    $UpdateSession = New-UpdateSession
    for (; $maxCycles -gt 0; $maxCycles -= 1) {
        Write-EventLogWrapper -message "Starting to check for updates. $maxCycles cycles remain."
    
        $CheckedUpdates = Check-WindowsUpdates -FilterInteractiveUpdates -UpdateSession $UpdateSession
        if (-not $CheckedUpdates) { 
            Write-EventLogWrapper "No applicable updates were detected. Done!" 
            break 
        }
    
        $RebootRequired = Install-WindowsUpdates -UpdateSession $UpdateSession -UpdateList $CheckedUpdates
        if ($RebootRequired) {
            Write-EventLogWrapper -message "Restart Required - Restarting..."
            Restart-ComputerAndUpdater -CyclesRemaining $maxCycles -NoRestart:$NoRestart 
        }
    }
    Run-PostUpdate
}

if ($MyInvocation.InvocationName -ne '.') {
    try {
        Run-WindowsUpdate
    }
    catch {
        $message  = "======== CAUGHT EXCEPTION ========`r`n$_`r`n"
        $message += "======== ERROR STACK ========`r`n"
        $error |% { $message += "$_`r`n----`r`n" }
        $message += "======== ========"
        Write-EventLogWrapper $message
        exit 666
    }
}

# Original version was 234 lines
