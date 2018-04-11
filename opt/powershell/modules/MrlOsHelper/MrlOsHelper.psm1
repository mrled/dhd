<#
.description
All OS-specific stuff should go in here
#>

function New-OsHelperObject {
    [CmdletBinding()] Param()

    $OsHelper = New-Object -TypeName PSObject

    switch (Get-PowershellPlatform) {
        "Win32NT" {
            Add-Member -InputObject $OsHelper -MemberType NoteProperty -Name SpecialFolders -Value (New-Object -TypeName PSObject)
            foreach ($sfName in [Enum]::GetValues([System.Environment+SpecialFolder])) {
                Add-Member -InputObject $OsHelper.SpecialFolders -MemberType NoteProperty -Name $sfName -Value [Environment]::GetFolderPath($sfName)
            }

            $startMenu = New-Object -TypeName PSObject -Property @{
                CurrentUser = "${env:AppData}\Microsoft\Windows\Start Menu\"
                AllUsers = "${env:ProgramData}\Microsoft\Windows\Start Menu\"
            }
            Add-Member -InputObject $OsHelper -MemberType NoteProperty -Name StartMenu -Value $startMenu
        }
    }

    return $OsHelper
}

<#
.description
Retrieve powershell platform (cross platform)
#>
function Get-PowershellPlatform {
    [CmdletBinding()] Param()
    if ($PSVersionTable.Keys -Contains 'Platform') {
        return $PsVersionTable.Platform
    } else {
        # This value is what is returned by $PsVersionTable.Platform on Powershell Core
        return "Win32NT"
    }
}

<#
.description
Test whether the current session has administrative privileges (cross platform)
#>
function Test-AdminRole {
    [CmdletBinding()] Param()
    if ((Get-PowershellPlatform) -eq "Win32NT") {
        $identity = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
        $adminRole = [Security.Principal.WindowsBuiltInRole] "Administrator"
        return $identity.IsInRole($adminRole)
    } else {
        return (id -u) -eq 0
    }
}

<#
.description
Retrieve the current machine's hostname (cross platform)
#>
function Get-Hostname {
    [CmdletBinding()] Param()
    if ($env:COMPUTERNAME) {
        return $env:COMPUTERNAME
    } else {
        return hostname
    }
}
