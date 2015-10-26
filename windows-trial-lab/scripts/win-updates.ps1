<#
.synopsis
Run Windows Update, installing all available updates and rebooting as necessary
.description
This script can be run directly, but it can also be dot-sourced to get access to internal functions without automatically checking for updates, applying them, or rebooting
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
TODO: Make sure every path of my program is writing unique log messages. Make sure all those paths are working.
TODO: Give pass event IDs for every event. ??
#>
param(
    [int] $MaxCycles = 5,
    [string] $ScriptProductName = "Marionettist",
    [string] $PostUpdateExpression,
    [switch] $NoRestart
)

$ErrorActionPreference = "Stop"
Set-StrictMode -version 2.0

# I need to get the path to this script from inside functions, which mess with the $MyInvocation variable
$script:ScriptPath = $MyInvocation.MyCommand.Path  

$script:RestartRegistryKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run"
$script:RestartRegistryProperty = "${ScriptProductName}InstallWindowsUpdates"

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
    Write-Host "====Writing to $ScriptProductName event log====`r`n$messagePlus`r`n"
    Write-EventLog -LogName $eventLogName -Source $ScriptProductName -EventID $eventId -EntryType $entryType -Message $MessagePlus    
}

<#
.synopsis
Create and set the registry property which will run this script on reboot 
#>
function Set-RestartRegistryEntry {
    param(
        [parameter(mandatory=$true)] [int] $CyclesRemaining,
        [string] $scriptPath = $script:ScriptPath
    )
    $psCall = @(
        "$PSHOME\powershell.exe"
        ('-File "{0}"' -f $scriptPath)
        "-MaxCycles $CyclesRemaining"
        "-ScriptProductName $ScriptProductName"
        ('-PostUpdateExpression "{0}"' -f "$PostUpdateExpression")
    )
    $message = "Setting registry key: {0}\{1}`r`n{2}" -f $script:RestartRegistryKey, $script:RestartRegistryProperty, ($psCall -join " ")
    Write-WinUpEventLog -message $message
    Set-ItemProperty -Path $script:RestartRegistryKey -Name $script:RestartRegistryProperty -Value ($psCall -join " ")
}

<#
.synopsis
If set, unset the registry property which would run this script on reboot 
#>
function Unset-RestartRegistryEntry {
    try {
        Get-ItemProperty $script:RestartRegistryKey | select $script:RestartRegistryProperty | out-null 
        Remove-ItemProperty -Path $script:RestartRegistryKey -Name $script:RestartRegistryProperty -ErrorAction SilentlyContinue 
    }
    catch {} # The key or entry did not exist
}

<#
.synopsis
Return the registry entry which runs this script on reboot, if present
#>
function Get-RestartRegistryEntry {
    try {
        return Get-ItemProperty $script:RestartRegistryKey | select $script:RestartRegistryProperty
    }
    catch {
        return $false # The key or entry did not exist
    } 
}

<#
.synopsis
Return a new Microsoft Update Session for use with Check-WindowsUpdates and Install-WindowsUpdates
#>
function New-UpdateSession {
    [cmdletbinding()] param()
    $UpdateSession = New-Object -ComObject 'Microsoft.Update.Session'
    $UpdateSession.ClientApplicationID = "$ScriptProductName Windows Update Installer"
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
.Description
Install Windows Updates. 
.parameter UpdateSession 
Must be a session created with New-UpdateSession - that is, a COM object of type Microsoft.Update.Session
.parameter UpdateList
A list of updates. Might be in a Microsoft.Update.UpdateColl COM object (possibly from Check-WindowsUpdates), or might be just a normal array of them
.outputs
Returns a boolean representing whether a reboot is required
#>
function Install-WindowsUpdates {
    [cmdletbinding()] param(
        [parameter(mandatory=$true)] $UpdateSession,
        [parameter(mandatory=$true)] $UpdateList
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
    while ((! $ok) -and ($attempts -lt 12)) {  # TODO: retry 12 times?? does this really need to be 12??
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
            $message += "`r`n> $($Update.Title)"
            $UpdatesToInstall.Add($Update) |Out-Null
        }
    }
    Write-WinUpEventLog -message $message

    $RebootRequired = $false

    if ($UpdatesToInstall.Count -gt 0) {
        $Installer = $UpdateSession.CreateUpdateInstaller()
        $Installer.Updates = $UpdatesToInstall
        $InstallationResult = $Installer.Install()
        $RebootRequired = [bool]$InstallationResult.RebootRequired

        $message =  "Windows Update complete. Installation Result: $($InstallationResult.ResultCode)."
        $message += "`r`nReboot Required: $RebootRequired"

        for($i=0; $i -lt $UpdatesToInstall.Count; $i++) {
            New-Object -TypeName PSObject -Property @{
                Title = $UpdatesToInstall.Item($i).Title
                Result = $InstallationResult.GetUpdateResult($i).ResultCode
            }
            $message += "`r`n`r`nItem: $($UpdatesToInstall.Item($i).Title)"
            $message += "`r`nResult: $($InstallationResult.GetUpdateResult($i).ResultCode)"
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
.outputs
If there applicable updates, return a Microsoft.Update.UpdateColl COM object; if not, return $null
.notes
After the updates are applied, this should run again because it may detect new updates.
#>
function Check-WindowsUpdates {
    param(
        [parameter(mandatory=$true)] $UpdateSession,
        [switch] $FilterInteractiveUpdates
    )
    Write-WinUpEventLog -message "Checking for Windows Updates at $(Get-Date)" -eventId 104
    $SearchResult = $null
    
    $UpdateSearcher = $UpdateSession.CreateUpdateSearcher()
    $successful = $FALSE
    $attempts = 0
    while(-not $successful -and $attempts -lt 12) { # TODO: retry 12 times?? does this really need to be 12??
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
    
    $ApplicableUpdates = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    foreach ($update in $SearchResult.Updates) {
        if ($FilterInteractiveUpdates) {
            try {
                $CanRequestUserInput = $Update.InstallationBehavior.CanRequestUserInput
            }
            catch {
                $CanRequestUserInput = $false
            }
            if ($CanRequestUserInput) {
                 Write-WinUpEventLog -message "Skipping: $($Update.Title) because it requires user input"
            }
            else {
                $ApplicableUpdates.Add($Update) | out-null
            }
        }
        else {
            $ApplicableUpdates.Add($Update) | out-null
        }
    }
   
    if ($ApplicableUpdates.Count -ne 0) {
        $message = "There are $($ApplicableUpdates.Count) remaining updates:`r`n"
        foreach ($update in $ApplicableUpdates) { 
            $message += "`r`n -  $($update.Title)"
        }
        Write-WinUpEventLog -message $message
    } 
    else {
        Write-WinUpEventLog -message 'There are no applicable updates'
    }
    return $ApplicableUpdates
}

<#
.synopsis
Run Windows Update, installing all available updates and rebooting as necessary
#>
function Run-WindowsUpdate {
    [cmdletbinding()] param()

    Unset-RestartRegistryEntry
    $UpdateSession = New-UpdateSession
    if ($maxCycles -ge 1) {
        foreach ($cycleCtr in 1..$maxCycles) {
            Write-WinUpEventLog -message "Starting to check for updates. Cycle $cycleCtr of maximum $maxCycles"
        
            $CheckedUpdates = Check-WindowsUpdates -FilterInteractiveUpdates -UpdateSession $UpdateSession
            if ((-not $CheckedUpdates) -or ($CheckedUpdates.Count -lt 1)) {
                Write-WinUpEventLog "No applicable updates were detected. Done!" 
                break 
            }
        
            $RebootRequired = Install-WindowsUpdates -UpdateSession $UpdateSession -UpdateList $CheckedUpdates
            if ($RebootRequired) {
                Write-WinUpEventLog -message "Restart Required - Restarting..."
                Set-RestartRegistryEntry -CyclesRemaining ($maxCycles - $cycleCtr)
                if ($NoRestart) { write-host "NoRestart: exiting instead"; exit 1; } 
                Restart-Computer -force 
            }
        }
    }
    else {
        Write-WinUpEventLog "We have reached our max cycle count - note that there may be outstanding updates"
    }
    Run-PostUpdate
}

if ($MyInvocation.InvocationName -ne '.') {
    Run-WindowsUpdate
}

# Original version was 234 lines