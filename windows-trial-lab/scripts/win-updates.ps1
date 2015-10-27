<#
.synopsis
Run Windows Update, installing all available updates and rebooting as necessary
.parameter MaxCycles
The number of times to check for updates before forcing a reboot, even if Windows Update has not indicated that one is required 
.parameter ScriptProductName
The name for this script that you want to make visible to the sysadmin. Could just be hard coded everywhere but I put it in the param() block for simpler customizability.
.parameter PostUpdateExpression
A string representing a PowerShell expression that is run one time after all updates have been applied (or MaxCycles has been reached without rebooting)
TODO: the parenthetical is nonobvious behavior and should be eliminated. In fact the whole of MaxCycles should be rethought. 
.parameter NoRestart
Print a message to the screen instead of actually restarting the computer; useful for debugging
.notes
This script can be run directly, but it can also be dot-sourced to get access to internal functions without automatically checking for updates, applying them, or rebooting
This script is intended to be 100% standalone because it needs to be able to tell Windows to call it again upon reboot. There are intentionally no dependencies, and features related to Windows Update that don't fit here (such as enable Microsoft Update) should live elsewhere
#>
param(
    [int] $MaxCycles = 5,
    [string] $ScriptProductName = "MarionettistWindowsUpdate",
    [string] $PostUpdateExpression,
    [string] [ValidateSet('RunBeforeLogon','RunAtLogon','NoRestart')] $RestartAction = "NoRestart",
    [switch] $CalledFromRegistry
)

<# Notes on the code: 

I'm trying to use StrictMode. That ends up making some code more complex than it would have to be otherwise. For instance, sometimes I have to check that a property exists (like $SomeVar.PSObject.Properties['PropertyName']) before I can use it. 

TODO: Make sure every path of my program is writing unique log messages. Make sure all those paths are working.
TODO: Pass unique event IDs for every event to Write-WinUpEventLog. ??
#>

$ErrorActionPreference = "Stop"
Set-StrictMode -version 2.0

# I need to get the path to this script from inside functions, which mess with the $MyInvocation variable
$script:ScriptPath = $MyInvocation.MyCommand.Path
$script:RestartRegistryKeys = @{
    RunBeforeLogon = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\RunServicesOnce"
    RunAtLogon = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce"
}
$script:RestartRegistryProperty = "$ScriptProductName"

<#
.synopsis
Write to a special event log, named after the $ScriptProductName. 
If that event log doesn't already exist, create it first.
#>
function Write-WinUpEventLog {
    param(
        [parameter(mandatory=$true)] [String] $message,
        [int] $eventId = 0,
        [ValidateSet("Error",'Warning','Information','SuccessAudit','FailureAudit')] $entryType = "Information"
    )
    $eventLogName = $ScriptProductName
    if (-not (get-eventlog -logname * |? { $_.Log -eq $eventLogName })) {
        New-EventLog -Source $ScriptProductName -LogName $eventLogName
    }
    $messagePlus = "$message`r`n`r`nScript: $($script:ScriptPath)`r`nUser: ${env:USERDOMAIN}\${env:USERNAME}"
    Write-Host -foreground magenta "====Writing to $ScriptProductName event log===="
    write-host -foreground darkgray "$messagePlus`r`n"
    Write-EventLog -LogName $eventLogName -Source $ScriptProductName -EventID $eventId -EntryType $entryType -Message $MessagePlus
}

<#
.synopsis
Create and set the registry property which will run this script on reboot 
#>
function Set-RestartRegistryEntry {
    param(
        [parameter(mandatory=$true)] [int] $CyclesRemaining,
        [parameter(mandatory=$true)] [ValidateSet('RunBeforeLogon','RunAtLogon','NoRestart')] [string] $RestartAction,
        [string] $scriptPath = $script:ScriptPath
    )

    if ($RestartAction -match "NoRestart") {
        Write-WinUpEventLog "Called Set-RestartRegistryEntry with -RestartAction NoRestart, will not write registry key" 
        return 
    }
    
    $psCallComponents = @(
        "$PSHOME\powershell.exe"
        ('-File "{0}"' -f $scriptPath)
        "-MaxCycles $CyclesRemaining"
        "-ScriptProductName $ScriptProductName"
        "-CalledFromRegistry"
        "-RestartAction ${script:RestartAction}"
        ('-PostUpdateExpression "{0}"' -f "$PostUpdateExpression")
    )
    $psCall = $psCallComponents -join " "
    $message = "Setting the Restart Registry Key at: {0}\{1}`r`n{2}" -f $script:RestartRegistryKeys.$RestartAction, $script:RestartRegistryProperty, $psCall
    Write-WinUpEventLog -message $message
    New-Item $script:RestartRegistryKeys.$RestartAction -force | out-null
    Set-ItemProperty -Path $script:RestartRegistryKeys.$RestartAction -Name $script:RestartRegistryProperty -Value $psCall
}

function Remove-RestartRegistryEntries {
    [cmdletbinding()] param()
    foreach ($key in $script:RestartRegistryKeys.Keys) { 
        try { Remove-ItemProperty -Path $script:RestartRegistryKeys[$key] -name $script:RestartRegistryProperty} catch {}
    }
}

function Restart-ComputerAndUpdater {
    param(
        [parameter(mandatory=$true)] [int] $CyclesRemaining,
        [parameter(mandatory=$true)] [ValidateSet('RunBeforeLogon','RunAtLogon','NoRestart')] [string] $RestartAction
    )
    Remove-RestartRegistryEntries
    if ($RestartAction -match "NoRestart") { 
        Write-WinUpEventLog "Restart-ComputerAndUpdater was called, but '-RestartAction NoRestart' was passed; exiting instead..."
        exit 1
    }
    else {
        Set-RestartRegistryEntry -CyclesRemaining $CyclesRemaining -RestartAction $RestartAction
        Write-WinUpEventLog "Rebooting..."
        Restart-Computer -Force
    }
}

<#
.synopsis
Return a new Microsoft Update Session for use with Check-WindowsUpdates and Install-WindowsUpdates
#>
function New-UpdateSession {
    [cmdletbinding()] param()
    $UpdateSession = New-Object -ComObject 'Microsoft.Update.Session'
    $UpdateSession.ClientApplicationID = "$ScriptProductName"
    return $UpdateSession
}

<#
.synopsis
Run the expression passed to this script with -PostUpdateExpression, if any
#>
function Run-PostUpdate {
    if ($PostUpdateExpression) {
        Write-WinUpEventLog -message "Running PostUpdate expression:`r`n`r`n${PostUpdateExpression}"
        Invoke-Expression $PostUpdateExpression
    }
    else {
        Write-WinUpEventLog -message "No PostUpdate to run"
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
        Write-WinUpEventLog "(Un)installation of a Windows component requires a restart" 
        return $true 
    }

    $WuauProp = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update"
    if ($WuauProp.PSObject.Properties['RebootRequired']) {
        Write-WinUpEventLog "Updates have been installed recently, but the machine must be restarted before installation is complete" 
        return $true 
    }
    
    Write-WinUpEventLog "There are no Windows Update -related reboots pending"
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
    
    if ((-not $UpdateList) -or ($UpdateList.Count -lt 1)) {
        Write-WinUpEventLog -message "No Updates To Download..."
        return $false
    }
    
    # Powershell does something weird to COM objects when passed between 
    # functions (maybe a scoping issue?) so even though $UpdateList may already
    # be an UpdateColl object (as you'd think would be the case if you got it 
    # from Check-WindowsUpdates), we have to create a new one here
    $UpdateComObject = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    $UpdateList |% { $UpdateComObject.Add($_) }
    
    Write-WinUpEventLog -message 'Downloading Updates...'
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
            Write-WinUpEventLog -message $message
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
    Write-WinUpEventLog -message $message

    $RebootRequired = $false
    if ($UpdatesToInstall.Count -gt 0) {
        $Installer = $UpdateSession.CreateUpdateInstaller()
        $Installer.Updates = $UpdatesToInstall
        
        if ($Installer.PSObject.Properties['RebootRequiredBeforeInstallation'] -and $Installer.RebootRequiredBeforeInstallation) {
            Write-WinUpEventLog "Reboot required before installation. (This can happen when updates are installed but the machine is not rebooted before trying to install again.)"
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
        Write-WinUpEventLog -message $message
    }
    else {
        Write-WinUpEventLog -message 'No updates available to install...'
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
    Write-WinUpEventLog -message "Checking for Windows Updates at $(Get-Date)" -eventId 104
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
            Write-WinUpEventLog -message $message
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
        $SkippedUpdates |% { $message += "`r`n -  $(_.Title)" } 
    }
    Write-WinUpEventLog -message $message
    
    return $ApplicableUpdates
}

<#
.synopsis
Run Windows Update, installing all available updates and rebooting as necessary
#>
function Run-WindowsUpdate {
    [cmdletbinding()] param()
    
    $message = "Running Windows Update "
    $message += if ($CalledFromRegistry) { "from the Restart Registry Entry" } else { "after being called directly" }
    Write-WinUpEventLog $message
    
    if (Get-RestartPendingStatus) {
        Restart-ComputerAndUpdater -CyclesRemaining $maxCycles -RestartAction $script:RestartAction 
    }
    
    $UpdateSession = New-UpdateSession
    for (; $maxCycles -gt 0; $maxCycles -= 1) {
        Write-WinUpEventLog -message "Starting to check for updates. $maxCycles cycles remain."
    
        $CheckedUpdates = Check-WindowsUpdates -FilterInteractiveUpdates -UpdateSession $UpdateSession
        if ((-not $CheckedUpdates) -or ($CheckedUpdates.Count -lt 1)) {
            Write-WinUpEventLog "No applicable updates were detected. Done!" 
            break 
        }
    
        $RebootRequired = Install-WindowsUpdates -UpdateSession $UpdateSession -UpdateList $CheckedUpdates
        if ($RebootRequired) {
            Write-WinUpEventLog -message "Restart Required - Restarting..."
            Restart-ComputerAndUpdater -CyclesRemaining $maxCycles -RestartAction $script:RestartAction
        }
    }

    Run-PostUpdate
}

if ($MyInvocation.InvocationName -ne '.') {
    Run-WindowsUpdate
}

# Original version was 234 lines