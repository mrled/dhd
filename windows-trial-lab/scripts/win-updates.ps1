<#
#>
param(
	[int] $MaxCycles = 5,
	[int] $global:MaxUpdatesPerCycle = 500,
	[string] $global:PostUpdateExpression
)

# TODO: Make sure every path of my program is writing unique log messages. Make sure all those paths are working.
# TODO: Give pass event IDs for every event. ??

function Run-PostUpdate {
	if ($global:PostUpdateExpression) {
		Write-WinUpEventLog -message "Running PostUpdate script at: ${global:PostUpdateExpression}"
		Invoke-Expression $global:PostUpdateExpression
	}
}
function Write-WinUpEventLog {
	param(
		[parameter(mandatory=$true)] [String] $message
        [int] $eventId = 0,
        [ValidateSet("Error",'Warning','Information','SuccessAudit','FailureAudit')] $entryType = "Information"
	)
	$eventLogName = "Marionettist"
	if (-not (get-eventlog -logname * |? { $_.Log -eq $eventLogName }))
    	New-EventLog -Source $MyInvocation.MyCommand.Path -LogName $eventLogName
	}
	$messagePlus = "$message`r`n`r`nScript: $($MyInvocation.MyCommand.Path)`r`nUser: ${env:USERDOMAIN}\${env:USERNAME}"
    Write-EventLog -LogName $eventLogName -Source $MyInvocation.MyCommand.Path -EventID $eventId -EntryType $entryType -Message $MessagePlus	
}
# TODO: redundancy in these 2 functions
function Remove-RestartRegistryKey {
    $RegistryKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run"
    $RegistryEntry = "MarionettistInstallWindowsUpdates"
	$psCall = "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -File $($MyInvocation.MyCommand.Path) -MaxUpdatesPerCycle ${global:MaxUpdatesPerCycle}"
	if ($global:PostUpdateExpression) { $psCall += " -PostUpdateExpression '$global:PostUpdateExpression'"}
	Set-ItemProperty -Path $RegistryKey -Name $RegistryEntry -Value $psCall
}
function Restart-ComputerAndUpdater {
    $RegistryKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run"
    $RegistryEntry = "MarionettistInstallWindowsUpdates"
	$prop = (Get-ItemProperty $RegistryKey).$RegistryEntry
	if ($prop) { Remove-ItemProperty -Path $RegistryKey -Name $RegistryEntry -ErrorAction SilentlyContinue }
	Restart-Computer
}

<#
.notes
Returns a boolean representing whether a reboot is required
#>
function Install-WindowsUpdates {
	[cmdletbinding()] param(
		[parameter(mandatory=$true)] [Microsoft.Update.Session] $UpdateSession,
		[parameter(mandatory=$true)] [Microsoft.Update.UpdateColl] $SearchResult,
	)
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

# TODO: Create the UpdateSession object outside of this function and pass it in 
# TODO: Do not return the UpdateSession object 
# TODO: Return the SearchResult object only 
# TODO: throw if a reboot is required - ??
# TODO: once rebooting is taken out of Check-WindowsUpdate, delete Restart-ComputerAndUpdater function and handle reboots in the caller
function Check-WindowsUpdates() {
	Write-WinUpEventLog -message "Checking for Windows Updates at $(Get-Date)" -eventId 104

	$RetVal = New-Object PSObject -Property {
		UpdateSession = $null
		SearchResult = $null
	}
	
	$RetVal.UpdateSession = New-Object -ComObject 'Microsoft.Update.Session'
	$RetVal.UpdateSession.ClientApplicationID = 'Marionettist Windows Update Installer'
    $UpdateSearcher = $RetVal.UpdateSession.CreateUpdateSearcher()
    $successful = $FALSE
    $attempts = 0
    $maxAttempts = 12
    while(-not $successful -and $attempts -lt $maxAttempts) {
        try {
            $RetVal.SearchResult = $UpdateSearcher.Search("IsInstalled=0 and Type='Software' and IsHidden=0")
            $successful = $TRUE
        } catch {
			$message = "Search call to UpdateSearcher was unsuccessful. Retrying in 10s.`r`n`r`n$($_.Exception | Format-List -force)"
			Write-WinUpEventLog -message $message
            $attempts += 1
            Start-Sleep -s 10
        }
    }
	
    if ($RetVal.SearchResult.Updates.Count -ne 0) {
        Write-WinUpEventLog -message "There are $($RetVal.SearchResult.Updates.Count) remaining updates"
        try {
            for($i=0; $i -lt $RetVal.SearchResult.Updates.Count; $i++) {
              	$message  = "Remaining Update: $($RetVal.SearchResult.Updates.Item($i).Title)"
              	$message += "`r`n$($RetVal.SearchResult.Updates.Description($i).Title)"
              	$message += "`r`n$($RetVal.SearchResult.Updates.RebootRequired($i).Title)"
              	$message += "`r`n$($RetVal.SearchResult.Updates.EulaAccepted($i).Title)"
				Write-WinUpEventLog -message $message
          	}
        } catch {
			$message = "Showing SearchResult was unsuccessful. Rebooting`r`n`r`n$($_.Exception | Format-List -force)"
			Write-WinUpEventLog -message $message
			# TODO: should return message that an error indicating restart was required
			Restart-ComputerAndUpdater
        }
    } else {
        Write-WinUpEventLog -message 'There are no applicable updates'
    }
	return $RetVal
}

Remove-RestartRegistryKey
for ($cycleCtr in 0..$maxCycles) {
	Write-WinUpEventLog -message "Starting to check for updates. Cycle $cycleCtr of maximum $maxCycles"
	$CheckedUpdates = Check-WindowsUpdates
	$RebootRequired = $false
	$DidUpdate = $false
	if ($CheckedUpdates.SearchResult.Updates.Count > 0) {
    	$RebootRequired = Install-WindowsUpdates -session $CheckedUpdates.UpdateSession
		$DidUpdate = $true
	}
	if ($RebootRequired) {
		Write-WinUpEventLog -message "Restart Required - Restarting..."
		Restart-ComputerAndUpdater
	}
	if (-not $didUpdate) { break }
}

Run-PostUpdate

# Original version was 234 lines