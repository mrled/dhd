[cmdletbinding()] param(
    [string] $newUsername = "mrled",
    [string] $newPassword = "boggy slab st droop speck"
)

$ErrorActionPreference = "Stop"

# REMINDER: This runs under the vagrant account, NOT under the account of the user I'll use to log in 

Get-Module |? -Property Name -match "wintriallab-postinstall" | Remove-Module
Import-Module $PSScriptRoot\wintriallab-postinstall.psm1

Invoke-ScriptblockAndCatch -scriptBlock {

    # Create my day-to-day user
    Add-LocalSamUser -userName "$newUsername" -password $newPassword
    Add-LocalSamUserToGroup -userName "$newUsername" -groupName "Administrators"
    Set-PasswordExpiry -accountName "$newUsername" -disable
    
    $secureNewPassword = ConvertTo-SecureString -String $newPassword -Force -AsPlainText
    $newAccountCreds = New-Object System.Management.Automation.PSCredential $newUsername,$secureNewPassword
    
    # Install apps
    $chocolateyPackages = @(
        "git.install"
        "terminals"
        "Firefox"
        "thunderbird"
        "ConEmu"
        "dropbox"
        "Everything"
        "sysinternals"
        "putty.install"
        "sublimetext3"
        "Emacs"
        "vim"
        "PsGet"
    )
    $chocolateyPackages |% { Invoke-ExpressionEx "choco install --yes '$_'" }
    
    Invoke-Command -ComputerName localhost -Creds $newAccountCreds -EnableNetworkAccess -ScriptBlock {
        Import-Module C:\vagrant\wintriallab-postinstall.psm1

        $suoParams = @{ 
            ShowHiddenFiles = $true
            ShowFileExtensions = $true
            ShowStatusBar = $true
            DisableSharingWizard = $true
            EnablePSOnWinX = $true
            EnableQuickEdit = $true
            DisableSystrayHide = $true
            DisableIEFirstRunCustomize = $true
        }
        Set-UserOptions @suoParams
        
        try { 
            $ErrorActionPreference = "Continue"
            Set-PinnedApplication -Action UnpinFromTaskbar -Filepath "C:\Program Files\WindowsApps\Microsoft.WindowsStore_2015.10.5.0_x86__8wekyb3d8bbwe\WinStore.Mobile.exe" -ErrorAction Continue
            Set-PinnedApplication -Action PinToTaskbar -Filepath "${env:ProgramFiles}\ConEmu\ConEmu.exe"
            #Set-PinnedApplication -Action PinToTaskbar -Filepath "${env:SystemRoot}\system32\eventvwr.msc"
            $UserPinnedTaskBar = "${env:AppData}\Microsoft\Internet Explorer\Quick Launch\User Pinned\TaskBar"
            if (test-path "$UserPinnedTaskBar\Server Manager.lnk") { rm "$UserPinnedTaskBar\Server Manager.lnk" }
        } 
        catch {}
        finally {
            $ErrorActionPreference = "Stop"
        }

    }
}
# missing apps: 
# - Discord
# - iCloud 
# - notepad2: broken, requires autoit anyway which isn't ideal. need to develop my own installer that doesn't replace notepad.exe, but tells all apps to use notepad2.exe
# - 1password

# Configure apps: 
# This is done BY HAND because I'm going to copy my %USERPROFILE% all the time. Right? 
# - dhd shortcuts
# - terminals config file 
# - evernote creds
# - dropbox creds
