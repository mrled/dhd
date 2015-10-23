<#
.parameter MaxCycles
The number of times to check for updates before forcing a reboot, even if Windows Update has not indicated that one is required 
.parameter MaxUpdatesPerCycle
The number of updates to apply at once
.parameter ScriptProductName
The name for this script that you want to make visible to the sysadmin. Could just be hard coded everywhere but I put it in the param() block for simpler customizability.
.parameter PostUpdateExpression
A string representing a PowerShell expression that is run one time after all updates have been applied (or MaxCycles has been reached without rebooting)
TODO: the parenthetical is nonobvious behavior and should be eliminated. In fact the whole of MaxCycles should be rethought. 
.parameter NoRestart
Print a message to the screen instead of actually restarting the computer
.notes
TODO: Make sure every path of my program is writing unique log messages. Make sure all those paths are working.
TODO: Give pass event IDs for every event. ??
#>
param(
    [int] $MaxCycles = 5,
    [int] $global:MaxUpdatesPerCycle = 500,
    [string] $global:ScriptProductName = "Marionettist",
    [string] $global:PostUpdateExpression,
    [switch] $NoRestart
)

$ErrorActionPreference = "Stop"
Set-StrictMode -version 2.0

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
    $messagePlus = "$message`r`n`r`nScript: $($script:MyInvocation.MyCommand.Path)`r`nUser: ${env:USERDOMAIN}\${env:USERNAME}"
    Write-Host "====Writing to $ScriptProductName event log====`r`n$messagePlus`r`n"
    Write-EventLog -LogName $eventLogName -Source $ScriptProductName -EventID $eventId -EntryType $entryType -Message $MessagePlus    
}

$script:RegistryKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run"
$script:RegistryEntry = "${ScriptProductName}InstallWindowsUpdates"
function Set-RestartRegistryKey {
    param(
        [parameter(mandatory=$true)] [int] $CyclesRemaining
    )
    $psCall = @(
        "$PSHOME\powershell.exe"
        "-File $($script:MyInvocation.MyCommand.Path)"
        "-MaxCycles $CyclesRemaining"
        "-MaxUpdatesPerCycle ${global:MaxUpdatesPerCycle}"
        "-ScriptProductName $ScriptProductName"
        ('-PostUpdateExpression "{0}"' -f "$PostUpdateExpression")
    )
    $message = "Setting registry key: {0}\{1}`r`n{2}" -f $script:RegistryKey, $script:RegistryEntry, ($psCall -join " ")
    Write-WinUpEventLog -message $message
    Set-ItemProperty -Path $script:RegistryKey -Name $script:RegistryEntry -Value ($psCall -join " ")
}
function Unset-RestartRegistryKey {
    try {
        Get-ItemProperty $script:RegistryKey | select $script:RegistryEntry | out-null 
        Remove-ItemProperty -Path $script:RegistryKey -Name $script:RegistryEntry -ErrorAction SilentlyContinue 
    }
    catch {} # The key or entry did not exist
}

function New-UpdateSession {
    [cmdletbinding()] param()
    $UpdateSession = New-Object -ComObject 'Microsoft.Update.Session'
    $UpdateSession.ClientApplicationID = "$ScriptProductName Windows Update Installer"
    return $UpdateSession
}

function Run-PostUpdate {
    if ($global:PostUpdateExpression) {
        Write-WinUpEventLog -message "Running PostUpdate script at: ${global:PostUpdateExpression}"
        Invoke-Expression $global:PostUpdateExpression
    }
}

function Restart-ComputerWrapper {
    if ($NoRestart) { 
        write-host "NoRestart: exiting instead"
        exit 1
    }
    else { 
        Restart-Computer -force 
    }
}

<#
.notes
Returns a boolean representing whether a reboot is required
#>
function Install-WindowsUpdates {
    [cmdletbinding()] param(
        #[parameter(mandatory=$true)] [Microsoft.Update.Session] $UpdateSession,   # wtf why does this break??
        #[parameter(mandatory=$true)] [Microsoft.Update.UpdateColl] $SearchResult  # wtf why does this break??
        [parameter(mandatory=$true)] $UpdateSession,
        [parameter(mandatory=$true)] $SearchResult
    )
    
    if ($SearchResult.Updates.Count -lt 1) {
        return $false
    }
    
    Write-WinUpEventLog -message "Evaluating Available Updates with limit of $($MaxUpdatesPerCycle):"
    $UpdatesToDownload = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    $updateCtr = 0
    $cycleUpdateCount = 0
    
    while (($updateCtr -lt $SearchResult.Updates.Count) -and ($cycleUpdateCount -lt $MaxUpdatesPerCycle)) {
        $Update = $SearchResult.Updates.Item($updateCtr)
        if ($Update -and (!$Update.IsDownloaded)) {
            if ($Update.InstallationBehavior.CanRequestUserInput) {
                Write-WinUpEventLog -message "Skipping: $($Update.Title) because it requires user input"
            } else {
                if (! $Update.EulaAccepted) { $Update.AcceptEula() }
                $cycleUpdateCount++
                Write-WinUpEventLog -message "Adding: $($Update.Title)"
                $UpdatesToDownload.Add($Update) |Out-Null
            }
        }
        $updateCtr++
    }

    if ($UpdatesToDownload.Count -eq 0) {
        Write-WinUpEventLog -message "No Updates To Download..."
    } else {
        Write-WinUpEventLog -message 'Downloading Updates...'
        $ok = $false
        $attempts = 0
        $maxAttempts = 12 # TODO: does this really need to be 12?
        while ((! $ok) -and ($attempts -lt $maxAttempts)) {
            try {
                $Downloader = $UpdateSession.CreateUpdateDownloader()
                $Downloader.Updates = $UpdatesToDownload
                $Downloader.Download()
                $ok = $true
            } catch {
                $message = "Error downloading updates. Retrying in 30s.`r`n`r`n$($_.Exception | Format-List -force)"
                Write-WinUpEventLog -message $message
                $attempts = $attempts + 1
                Start-Sleep -s 30
            }
        }
    }

    $UpdatesToInstall = New-Object -ComObject 'Microsoft.Update.UpdateColl'
    $message = 'The following updates are downloaded and ready to be installed:'
    foreach ($Update in $SearchResult.Updates) {
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
.notes
After the updates are applied, this should run again because it may detect new updates.

TODO: Can't figure out a way to check for just my specific specific exception that indicates a reboot. 
This means that ANY uncaught error from this function will be interpreted by the caller as saying that a reboot is required!
#>
function Check-WindowsUpdates {
    param(
        #[parameter(mandatory=$true)] [Microsoft.Update.Session] $UpdateSession # ?? wtf why does this break?
        [parameter(mandatory=$true)] $UpdateSession,
        [switch] $FilterInteractiveUpdates
    )
    Write-WinUpEventLog -message "Checking for Windows Updates at $(Get-Date)" -eventId 104
    $SearchResult = $null
    
    $UpdateSearcher = $UpdateSession.CreateUpdateSearcher()
    $successful = $FALSE
    $attempts = 0
    $maxAttempts = 12
    while(-not $successful -and $attempts -lt $maxAttempts) {
        try {
            $SearchResult = $UpdateSearcher.Search("IsInstalled=0 and Type='Software' and IsHidden=0")
            $successful = $TRUE
        } 
        catch {
            $message = "Search call to UpdateSearcher was unsuccessful. Retrying in 10s.`r`n`r`n$($_.Exception | Format-List -force)"
            Write-WinUpEventLog -message $message
            $attempts += 1
            Start-Sleep -s 10
        }
    }
    
    if ($SearchResult.Updates.Count -ne 0) {
        Write-WinUpEventLog -message "There are $($SearchResult.Updates.Count) remaining updates"
        try {
            for($i=0; $i -lt $SearchResult.Updates.Count; $i++) {
                $message  = "Remaining Update: $($SearchResult.Updates.Item($i).Title)"
                # None of these seem to work in Strict Mode??? 
                #$message += "`r`n$($SearchResult.Updates.Description($i).Title)"
                #$message += "`r`n$($SearchResult.Updates.RebootRequired($i).Title)"
                #$message += "`r`n$($SearchResult.Updates.EulaAccepted($i).Title)"
                Write-WinUpEventLog -message $message
            }
        } 
        catch {
            $message = "Showing SearchResult was unsuccessful because a reboot is required`r`n`r`n$($_.Exception | Format-List -force)"
            Write-WinUpEventLog -message $message
            $exc = New-Object System.InvalidOperationException "Showing SearchResult was unsuccessful because a reboot is required"
            $err = New-Object Management.Automation.ErrorRecord $exc,0,"InvalidOperation",$null
            $PSCmdlet.ThrowTerminatingError($err)
        }
    } 
    else {
        Write-WinUpEventLog -message 'There are no applicable updates'
    }
    return $SearchResult
}

Unset-RestartRegistryKey
$UpdateSession = New-UpdateSession
foreach ($cycleCtr in 1..$maxCycles) {
    Write-WinUpEventLog -message "Starting to check for updates. Cycle $cycleCtr of maximum $maxCycles"
    $CyclesRemaining = $maxCycles - $cycleCtr

    try {
        $CheckedUpdates = Check-WindowsUpdates -UpdateSession $UpdateSession
    }
    catch {
        Set-RestartRegistryKey -CyclesRemaining $CyclesRemaining
        Restart-ComputerWrapper
    }

    if ($CheckedUpdates.Updates.Count -lt 1) { break } # No updates, we're done

    $RebootRequired = Install-WindowsUpdates -UpdateSession $UpdateSession -SearchResult $CheckedUpdates
    if ($RebootRequired) {
        Write-WinUpEventLog -message "Restart Required - Restarting..."
        Set-RestartRegistryKey -CyclesRemaining $CyclesRemaining
        Restart-ComputerWrapper 
    }
}

Run-PostUpdate

# Original version was 234 lines