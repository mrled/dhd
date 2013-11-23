
function invoke-magic {

    $Me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    $SoyAdmin= $Me.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")

    if ($soyadmin) { $sepscope = "localmachine" }
    else { $sepscope = "currentuser" }
    set-executionpolicy unrestricted -scope $sepscope -force
    if ([Environment]::Is64BitProcess) {
        start-job { Set-ExecutionPolicy unrestricted -scope $sepscope -force } -RunAs32
    }
    $magicJobs = @()

    $erroractionpreference = "silentlycontinue"
    $magicJobs += @(Register-EngineEvent -SourceIdentifier DhdCloned -Action {
        copy ~/.dhd/opt/win32/ConEmu.xml $env:appdata
        mkdir ~/Documents/WindowsPowerShell -force | out-null
        $dhdprofile = "~/.dhd/hbase/Microsoft.Powershell_profile.win32.ps1"
        copy $dhdprofile ~/Documents/WindowsPowerShell/Microsoft.Powershell_profile.ps1
        . $dhdprofile
        reinit # from my initialization.ps1 file in .dhd
    })
    $magicJobs += @(Register-EngineEvent -SourceIdentifier GitInstalled -Action {
        Register-EngineEvent -SourceIdentifier DhdCloned -Forward
        foreach ($maybegit in "C:\Program Files (x86)\git\cmd\git.exe","C:\Program Files\git\cmd\git.exe") {
            if (test-path $maybegit) {
                set-alias git $maybegit
            }
        }
        if (-not (test-path alias:/git)) {
            write-error "Can't find git - where was it installed?"
        }
        if (test-path $home/.dhd) {
            rm -force -recurse $home/.dhd
        }
        git clone https://github.com/mrled/dhd.git $home/.dhd
        $null = New-Event -SourceIdentifier DhdCloned
    })
    $magicJobs += @(Register-EngineEvent -SourceIdentifier PsgetInstalled -Action {
        import-module "C:\Program Files\Common Files\Modules\PsGet\PsGet.psm1"
        install-module "PSReadLine"
        install-module "TabExpansion++"
        install-module "PSCX"
    })

    $magicJobs += @(Register-EngineEvent -SourceIdentifier ChocolateyInstalled -Action { 
        Register-EngineEvent -SourceIdentifier GitInstalled -Forward
        Register-EngineEvent -SourceIdentifier PsgetInstalled -Forward
        Register-EngineEvent -SourceIdentifier ConemuInstalled -Forward
        cinst git.install 
        $null = New-Event -SourceIdentifier GitInstalled
        cinst psget
        $null = New-Event -SourceIdentifier PsgetInstalled
        cinst conemu
        $null = New-Event -SourceIdentifier ConemuInstalled
    })

    $magicJobs += @(start-job -name InstallChocolatey -scriptblock {
        Register-EngineEvent -SourceIdentifier ChocolateyInstalled -Forward
        iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
        [Environment]::SetEnvironmentVariable("path", $env:path+";${env:systemdrive}\chocolatey\bin", "User")
        if ($SoyAdmin) {
            [Environment]::SetEnvironmentVariable("path", $env:path+";${env:systemdrive}\chocolatey\bin", "User")
        }
        $null = New-Event -SourceIdentifier ChocolateyInstalled
    })
    $erroractionpreference = "stop"
    return $magicJobs
}

function Show-StatusLoop {
    param(
        [parameter(mandatory=$true)] $jobs
    )
    write-host "Casting magical spells and whatever bullshit..."
    $complete = $false
    $counter = 0
    while (-not $complete) {
        $anyFailed = $anyUnstarted = $false
        foreach ($job in $jobs) {
            switch ($job.state) {
                Failed {
                    $anyFailed = $true
                    $stateColor = "red"
                }
                NotStarted {
                    $anyUnstarted = $true
                    $stateColor = "yellow"
                }
                Running {
                    $stateColor = "blue"
                }
                Completed {
                    $stateColor = "green"
                }
                default {
                    $stateColor = "white"
                }
            }
            write-host -nonewline "$counter " -foreground magenta
            write-host -nonewline $job.name
            write-host -nonewline ": "
            write-host $job.state -foreground $stateColor
        }
        if ($anyFailed -or (-not $anyUnstarted)) { $complete = $true }
        $counter += 1
        sleep 2
    }
    write-host "========"
    foreach ($job in $jobs) {
        write-host "Job: $($job.name): " -nonewline 
        write-host $job.state
        if ($job.output) {
            write-host "$($job.name) Output: " 
            write-host $job.output -foreground green
        }
        if ($job.warning) {
            write-host "$($job.name) Warning: "
            write-host $job.warning -foreground yellow
        }
        if ($job.error) {
            write-host "$($job.name) Error: "
            write-host $job.error -foreground red
        }
    }
    write-host "========"
    if ($anyFailed) {
        write-host "~*~ SPELL CASTING WAS UNSUCCESSFUL ~*~" -foreground red
    }
    else {
        write-host "~*~ SPELL CASTING WAS SUCCESSFUL ~*~" -foreground green
    }
}

# $magicJobs = invoke-magic 
# Show-StatusLoop $magicJobs
