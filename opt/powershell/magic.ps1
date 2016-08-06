<#
.description
Set up a new Windows machine
.example
Invoke-WebRequest -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/powershell/magic.ps1 | Invoke-Expression

Must be run from an administrative prompt, unless Chocolatey and everything we install from it is already present
#>

<#
.parameter name
The name of the task (for logging)
.parameter action
A scriptblock to invoke
.parameter condition
A boolean; if it evaluates to $true, run the action; otherwise, skip the action
.description
Perform an action, logging it with a name.
#>
function Invoke-MagicStep {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] [String] $name,
        [Parameter(Mandatory=$true)] [ScriptBlock] $action,
        [Bool] $condition = $true
    )
    if ($condition) {
        Write-Host "Invoking action '$name'..."
        Invoke-Command -ScriptBlock $action
    }
    else {
        Write-Host "Skipping action '$name'"
    }
}

<#
.description
Determine
#>
function Test-CommandInPath {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] $commandName
    )
    foreach ($path in ($env:path -split ';' | Sort-Object -Unique)) {
        $pathToTest = "$path\$commandName"
        if (Test-Path $pathToTest) {
            Write-Verbose $pathToTest
            return $true
        }
    }
    return $false
}

function Test-AdministratorRole {
    $me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    $SoyAdmin = $me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
}

function Set-ExecPolUnrestricted {
    $sepScope = if (Test-AdministratorRole) { "LocalMachine" } else { "CurrentUser" }
    Set-ExecutionPolicy Unrestricted -Scope $sepScope -Force
    if ([Environment]::Is64BitProcess) {
        Start-Job { Set-ExecutionPolicy Unrestricted -Scope $sepScpope -Force } -RunAs32
    }
}

function Install-ConEmuConfiguration {
    $conEmuXmlPath = "${env:AppData}\ConEmu.xml"
    $conEmuXmlBakPath = "${conEmuXmlPath}.magic.bak"
    if (Test-Path $conEmuXmlPath) {
        if (-not (Test-Path $conEmuXmlBakPath)) {
            # Under normal circumstances, the backup path won't exist; store a backup of the ConEmu XML file just in case
            Move-Item $conEmuXmlPath $conEmuXmlBakPath
        }
        else {
            # If the backup path exists, assume we've run before and just delete the ConEmu XML file
            Remove-Item $conEmuXmlPath
        }
    }
    Copy-Item $Home\.dhd\opt\win32\ConEmu.xml $conEmuXmlPath
}

function Install-PowershellProfile {
    mkdir -Force (Split-Path $Profile) | Out-Null
    $dhdProfile = "~/.dhd/hbase/Microsoft.Powershell_profile.win32.ps1"
    copy "$Home\.dhd\hbase\Microsoft.Powershell_profile.win32.ps1" $Profile -Force
}

function Invoke-PowershellProfile {
    . $Profile
    reinit # from my initialization.ps1 file in .dhd
}

function Install-Chocolatey {
    Invoke-WebRequest https://chocolatey.org/install.ps1 -UseBasicParsing | Invoke-Expression
}

function Install-PsGet {
    (New-Object Net.WebClient).DownloadString("http://psget.net/GetPsGet.ps1") | Invoke-Expression
}

$ErrorActionPreference = "Stop"
Set-ExecPolUnrestricted

# Tasks that require administrative privileges:
Invoke-MagicStep "install chocolatey" {Install-Chocolatey} (-not (Test-CommandInPath choco.exe))
Invoke-MagicStep "install git" {choco.exe install git.install} (-not (Test-CommandInPath git.exe))
Invoke-MagicStep "install conemu" {choco.exe install conemu} (-not (Test-Path "${env:ProgramFiles}\ConEmu\ConEmu.exe"))

# Remaining tasks do not require admin privs
Invoke-MagicStep "install psget" {Install-PsGet} (-not (Get-Module -ListAvailable PsGet)) 
Invoke-MagicStep "checkout dhd" {git.exe checkout https://github.com/mrled/dhd.git $Home\.dhd} (-not (Test-Path $Home\.dhd)) 
Invoke-MagicStep "install conemu config" {Install-ConEmuConfiguration}
Invoke-MagicStep "install powershell profile" {Install-PowershellProfile}
# Invoke-MagicStep "install psreadline" {Install-Module PSReadLine}
Invoke-MagicStep "invoke powershell profile" {Invoke-PowershellProfile}
