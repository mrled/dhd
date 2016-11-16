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
    # Define potential PATH elements in the order you want them to appear in the final PATH
    # Remember that the Machine PATH is prepended to this list
    # Note that relative paths are parsed as if they are a subdirectory of %ProgramFiles% or %ProgramFiles(x86)%
    $possiblePaths = @(
        # Important things that must come first
        'vim\vim*'
        'vim\vim*\macros'
        "${env:ChocolateyInstall}\bin"
        "${env:SystemDrive}\ProgramData\Chocolatey\bin"

        # Everything else should go in alphabetical order grouped by type
        "${env:SystemDrive}\Perl64"
        "${env:SystemDrive}\Python3*"
        "${env:SystemDrive}\Python3*\Scripts"
        "${env:SystemDrive}\Tools\Go\bin"
        "${env:SystemDrive}\Tools\mingw64\bin"
        "${env:SystemDrive}\Tools\Python3*"
        "${env:SystemDrive}\Tools\Python3*\Scripts"
        "${env:SystemDrive}\Tools\Ruby*\bin"
        "${env:AppData}\npm"
        "${env:LocalAppData}\Pandoc"
        "$Home\.dhd\opt\powershell\bin"
        "$Home\opt\bin"
        '7-zip'
        'ConEmu'
        'ConEmu\ConEmu'
        'Git\cmd'
        'GNU\GnuPG'
        'Graphviz*\bin'
        'LLVM\bin'
        'Microsoft Visual Studio*\VC\bin'
        'MSBuild\*\Bin'
        'OpenSSL\bin'
        'Oracle\VirtualBox'
        'Nmap'
        'PuTTY'
        'Sublime Text 3'
    )

    # Set the "User" PATH - that is, the PATH for future processed launched by this same user - first
    # Might as well test for existing User and Process PATH values as well
    # No reason to check the "Machine" PATH here, because we are going to set the "User" PATH to this value, and the "Machine" PATH is prepended to that at process start time anyway
    $possibleUserPaths = @(
        $possiblePaths
        [Environment]::GetEnvironmentVariable("PATH", "User") -split ';'
        [Environment]::GetEnvironmentVariable("PATH", "Process") -split ';'
    )
    $resolvedUserPaths = Resolve-PotentialExecutablePathList $possibleUserPaths
    Set-ExecutablePath -path ($resolvedUserPaths -join ';') -target User      # Persist PATH changes so that future processes see them

    # Now we set the "Process" PATH - that is, the PATH for the rest of the lifetime of this process.
    # Prepend the "Machine" PATH here too, so we don't lose something important that might be set there and we pick up any changes that may have happened since our current process launched
    $possibleProcessPaths = @(
        [Environment]::GetEnvironmentVariable("PATH", "Machine") -split ';'
        $possibleUserPaths
    )
    $resolvedProcessPaths = Resolve-PotentialExecutablePathList $possibleProcessPaths
    Set-ExecutablePath -path ($resolvedProcessPaths -join ';') -target Process   # Update this process's PATH, so that we can use the new locations immediately
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
    Setup-SystemPath
    Setup-Environment
}
