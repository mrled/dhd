# Initialization stuff doesn't need to happen very often, and it might be slow ish maybe

<#
.notes
This function is necessary for "superpack" installers of Python modules
They're fuckin dumb and they only look in HKCU not HKLM
See also: http://stackoverflow.com/q/3008509/868206
UPDATE 2016-11-11: is this still necessary? This may be a relic of the past at this point...
#>
function Setup-PythonRegistryKeys {
    param(
        [switch] $force
    )
    $hkcuPath = "HKCU:\SOFTWARE\Python"
    $hklmPath = "HKLM:\Software\Python"

    if (test-path $hklmPath) {
        if ($force -or -not (test-path $hkcuPath)) {
            copy-item -recurse $hklmPath $hkcuPath -force:$force
        }
    }
}

function Setup-SystemPath {
    <#
    Some notes about this: 
    - You want Git to be very high in the path list, because it can work around the stupid rebase.exe problem
      (http://stackoverflow.com/questions/18502999/git-extensions-win32-error-487-couldnt-reserve-space-for-cygwins-heap-win32)
    #>
    $possiblePaths = @(
        "${env:SystemDrive}:\ProgramData\Chocolatey\bin"
        "${env:SystemDrive}:\Tools\mingw64\bin"
        "${env:SystemDrive}:\Tools\Python3*"
        "${env:SystemDrive}:\Tools\Python3*\Scripts"
        "${env:SystemDrive}:\Python3*"
        "${env:SystemDrive}:\Python3*\Scripts"
        "${env:SystemDrive}:\Tools\Ruby*\bin"
        "${env:SystemDrive}:\Perl64"
        "${env:ChocolateyInstall}\bin"
        "$Home\.dhd\opt\powershell\bin"
        "$Home\opt\bin"
        "$Home\AppData\Roaming\npm"
    )

    $possibleProgramFilesPaths  = @(
        'Git\cmd'
        'PuTTY'
        '7-zip'
        'Nmap'
        'LLVM\bin'
        'GNU\GnuPG'
        'Microsoft Visual Studio*\VC\bin'
    )
    $possiblePaths += @($possibleProgramFilesPaths |% { Get-ProgramFilesChild $_ })

    # If there's any directory in the existing paths, might as well test for it
    $possiblePaths += [Environment]::GetEnvironmentVariable("PATH", "Machine") -split ';'
    $possiblePaths += [Environment]::GetEnvironmentVariable("PATH", "User") -split ';'
    $possiblePaths += [Environment]::GetEnvironmentVariable("PATH", "Process") -split ';'

    # .description
    # Take an array of paths, which may contain wildcards, and resolve and return the first one that exists on the filesystem
    function Get-PathIfExists {
        Param([Parameter(Mandatory=$True)] [String[]] $path)
        $path |% {if (Test-Path $_) {return Resolve-Path $_ | Select -Last 1 -Expand Path}}
    }

    $existingPaths = @()
    $possiblePaths |% {if ($_) {Get-PathIfExists $_}} | Sort-Object -Unique |% {if ($_) {$existingPaths+=@($_)}}

    $path = $existingPaths -join ';'
    Set-ExecutablePath -path $path -target User      # Persist PATH changes so that future processes see them
    Set-ExecutablePath -path $path -target Process   # Update this process's PATH, so that we can use the new locations immediately
}

function Setup-Environment {
    # Term is important for things like less.exe, sometimes
    Set-WinEnvironmentVariable -name "TERM" -value "xterm" -targetlocation user,process

    Set-FileAssociation .el txtfile
    Set-FileAssociation .nfo txtfile "text/plain"
    Set-FileAssociation .mdwn txtfile "text/plain"
    Set-FileAssociation .markdown txtfile "text/plain"
    Set-FileAssociation .md txtfile "text/plain"
    Set-FileAssociation .text txtfile "text/plain"
    Set-FileAssociation .mkd txtfile "text/plain"

    Set-WinEnvironmentVariable -name PYTHONSTARTUP -value "$home\.dhd\hbase\python.profile" -targetlocation user,process
    Set-WinEnvironmentVariable -name PythonPath -value "$Home\.dhd\opt\python" -targetlocation user,process
}

# The closest I can get to my .b() bash function is dot-sourcing this function: `. p`
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
}

function reinit {
    . p
    Setup-SystemPath
    Setup-Environment
}
