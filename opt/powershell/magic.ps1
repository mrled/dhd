$erroractionpreference = "stop"

function invoke-magic {

    $Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    $SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")

    if ($soyadmin) { $sepscope = "localmachine" }
    else { $sepscope = "currentuser" }
    set-executionpolicy unrestricted -scope $sepscope -force
    if ([Environment]::Is64BitProcess) {
        start-job { Set-ExecutionPolicy unrestricted -scope $sepscope -force } -RunAs32
    }

    $dhdClonedEv = Register-EngineEvent -SourceIdentifier DhdCloned -Action {
        copy ~/.dhd/opt/win32/ConEmu.xml $env:appdata
        mkdir ~/Documents/WindowsPowerShell -force | out-null
        $dhdprofile = "~/.dhd/hbase/Microsoft.Powershell_profile.win32.ps1"
        copy $dhdprofile ~/Documents/WindowsPowerShell/Microsoft.Powershell_profile.ps1
        . $dhdprofile
        reinit # from my initialization.ps1 file in .dhd
    }
    $gitInstalledEv = Register-EngineEvent -SourceIdentifier GitInstalled -Action {
        Register-EngineEvent -SourceIdentifier DhdCloned -Forward
        foreach ($maybegit in "C:\Program Files (x86)\git\cmd\git.exe","C:\Program Files\git\cmd\git.exe") {
            if (test-path $maybegit) {
                set-alias git $maybegit
            }
        }
        if (-not (test-path alias:/git)) {
            write-error "Can't find git - where was it installed?"
        }
        git clone https://github.com/mrled/dhd.git $home/.dhd
        $null = New-Event -SourceIdentifier DhdCloned
    }
    $psgetInstalledEv = Register-EngineEvent -SourceIdentifier PsgetInstalled -Action {
        import-module psget
        install-module PSReadLine,TabExpansion++
    }

    $chocolateyInstalledEv = Register-EngineEvent -SourceIdentifier ChocolateyInstalled -Action { 
        Register-EngineEvent -SourceIdentifier GitInstalled -Forward
        Register-EngineEvent -SourceIdentifier PsgetInstalled -Forward
        Register-EngineEvent -SourceIdentifier ConemuInstalled -Forward
        cinst git.install 
        $null = New-Event -SourceIdentifier GitInstalled
        cinst psget
        $null = New-Event -SourceIdentifier PsgetInstalled
        cinst conemu
        $null = New-Event -SourceIdentifier ConemuInstalled
    }

    $ChocolateyJob = start-job -name InstallChocolatey -scriptblock {
        Register-EngineEvent -SourceIdentifier ChocolateyInstalled -Forward
        iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
        [Environment]::SetEnvironmentVariable("path", $env:path+";${env:systemdrive}\chocolatey\bin", "User")
        if ($SoyAdmin) {
            [Environment]::SetEnvironmentVariable("path", $env:path+";${env:systemdrive}\chocolatey\bin", "User")
        }
        $null = New-Event -SourceIdentifier ChocolateyInstalled
    }

    return $ChocolateyJob
}


# get console and settings from dropbox