<#
.DESCRIPTION
Configure the user's settings for Windows and third party software
#>

Configuration UserSettingsConfig {
    param(
        [string[]] $ComputerName = "localhost",
        [string] $AppData = "${env:AppData}",
        [string] $LocalAppData = "${env:LocalAppData}",
        [Parameter(Mandatory)] [PSCredential] $Credential
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlUserEnvironment
    Import-DscResource -ModuleName cMrlGitGlobalConfiguration
    Import-DscResource -ModuleName cMrlPathLikeEnvVar
    Import-DscResource -ModuleName cMrlPathLikeEnvVarSet
    Import-DscResource -ModuleName xComputerManagement

    Node $ComputerName {

        Registry "DisallowAppsFromUsingAdvertisingId" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo"
            ValueName = "Enabled"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "DisableBingSearchResultsInStartMenu" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Search"
            ValueName = "BingSearchEnabled"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowHiddenFiles" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "Hidden"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowFileExtensions" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "HideFileExt"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "SetExplorerHomeScreenToThisPc" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "LaunchTo"
            ValueData = 1  # Default value: 2
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ExpandExplorerNavigationPaneToCurrentFolder" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "NavPaneExpandToCurrentFolder"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowAllFoldersInExplorerNavigationPane" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "NavPaneShowAllFolders"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "ShowExplorerStatusBar" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "ShowStatusBar"
            ValueData = 1
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "DisableSharingWizard" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced"
            ValueName = "SharingWizardOn"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "DisablePeopleInTaskbar" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\People"
            ValueName = "PeopleBand"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }
        Registry "NeverHideSystemTrayIcons" {
            Key = "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer"
            ValueName = "EnableAutoTray"
            ValueData = 0
            ValueType = "Dword"
            PsDscRunAsCredential = $Credential
        }

        # Important for things like less.exe, sometimes
        cMrlUserEnvironment "SetTermEnvVar" {
            Name = "TERM"
            Value = "xterm"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }

        # Less command-line arguments
        # -i: Ignore case in searches that do not contain uppercase.
        # -R: Output "raw" control characters.
        # -c: Repaint by clearing rather than scrolling.
        # -F: Quit if entire file fits on first screen. (Don't use with -c, lol)
        cMrlUserEnvironment "SetLessEnvVar" {
            Name = "LESS"
            Value = "-iRc"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }

        # Set PAGER, useful for things like the `aws` CLI
        cMrlUserEnvironment "SetPagerEnvVar" {
            Name = "PAGER"
            Value = "less.exe"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }

        # LESSOPEN is not always set with security in mind
        # See also: https://marc.info/?l=full-disclosure&m=141678420425808&w=2
        cMrlUserEnvironment "SetLessopenEnvVar" {
            Name = "LESSOPEN"
            PsDscRunAsCredential = $Credential
            Ensure = "Absent"
        }

        # I think this is less necessary recently, but some unix software does still use this
        cMrlUserEnvironment "SetHomeEnvVar" {
            Name = "HOME"
            Value = "$env:USERPROFILE"
            PsDscRunAsCredential = $Credential
            Ensure = "Present"
        }

        cMrlGitGlobalConfiguration "GitSetUsername" {
            Name = "user.name"
            Value = "Micah R Ledbetter"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        cMrlGitGlobalConfiguration "GitSetUserEmail" {
            Name = "user.email"
            Value = "me@micahrl.com"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # By default, 'git branch' commands are piped to a pager, which is annoying
        cMrlGitGlobalConfiguration "GitDisablePagerForBranchCommand" {
            Name = "pager.branch"
            Value = "false"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # Requires subl.exe to be in $env:PATH
        cMrlGitGlobalConfiguration "GitConfigureSublimeTextAsEditor" {
            Name = "core.editor"
            Value= "subl.exe -w"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # As of 20180221, the "Less" Chocolatey package is version 529
        # - No problems refreshing screen
        # - Displays colors from ANSI escape sequences properly
        # - It's much faster than relying on vim's less.vim package
        # Requires less.exe to be in $env:PATH
        cMrlGitGlobalConfiguration "GitConfigureLessPager" {
            Name = "core.pager"
            Value = "${env:ChocolateyInstall}\bin\less.exe" -Replace "\\","\\"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # If we have an ssh.exe in the path, assume it's Microsoft's OpenSSH port
        # - Lets us use ~/.ssh/config, ~/.ssh/id_* keys, etc
        # - Tested and works well
        # Requires ssh.exe to be in $env:PATH
        cMrlGitGlobalConfiguration "GitConfigureSsh" {
            Name = "core.sshCommand"
            Value = "ssh.exe"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        # I don't remember if this gets set by default?
        cMrlUserEnvironment "SetGoPathEnvVar" {
            Name = "GOPATH"
            Value = "$env:USERPROFILE\Documents\Go"
            Ensure = "Present"
            PsDscRunAsCredential = $Credential
        }

        cMrlPathLikeEnvVarSet "PrependPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "$env:USERPROFILE\opt\bin"
                "$LocalAppData\Continuum\miniconda3"
                "$LocalAppData\Continuum\miniconda3\Scripts"
            )
            Ensure = "Present"
            InsertionMode = "Prepend"
            OnlyIfExists = $true
            PsDscRunAscredential = $Credential
        }

        cMrlPathLikeEnvVarSet "PrependPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "${AppData}\npm"
                "${AppData}\Python\Python3*\Scripts"
                "${AppData}\Python\Python2*\Scripts"
                "${LocalAppData}\atom\bin"
                "${LocalAppData}\Pandoc"
                "${LocalAppData}\Keybase"
                "${env:GOROOT}\bin"
                "${env:GOPATH}\bin"
            )
            Ensure = "Present"
            InsertionMode = "Prepend"
            OnlyIfExists = $true
            PsDscRunAscredential = $Credential
        }

        xScheduledTask AddRepoHelperDriveLimited {
            TaskName = 'Configure R drive (limited)'
            # TaskPath = '\'
            ActionExecutable = 'cmd.exe'
            ActionArguments = '/c IF EXIST "%USERPROFILE%\Documents\Repositories" ( subst.exe R: "%USERPROFILE%\Documents\Repositories" )'
            ScheduleType = 'AtLogOn'
            Enable = $true
            User = $Credential.UserName
            LogonType = 'Interactive'
            RunLevel = 'Limited'
            ExecuteAsCredential = $Credential
        }
        xScheduledTask AddRepoHelperDriveElevated {
            TaskName = 'Configure R drive (elevated)'
            # TaskPath = '\'
            ActionExecutable = 'cmd.exe'
            ActionArguments = '/c IF EXIST "%USERPROFILE%\Documents\Repositories" ( subst.exe R: "%USERPROFILE%\Documents\Repositories" )'
            ScheduleType = 'AtLogOn'
            Enable = $true
            User = $Credential.UserName
            LogonType = 'Interactive'
            RunLevel = 'Highest'
            ExecuteAsCredential = $Credential
        }

    }
}
