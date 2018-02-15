# Initialization stuff doesn't need to happen very often, and it might be slow ish maybe

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
        "${env:LocalAppData}\Continuum\miniconda3"
        "${env:LocalAppData}\Continuum\miniconda3\Scripts"

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
        "${env:LocalAppData}\atom\bin"
        "${env:LocalAppData}\Pandoc"
        "${env:LocalAppData}\Keybase"

        # NOTE: Should use $Home, which expands properly, NOT ~, which doesn't work with Resolve-PotentialExecutablePathList
        "$Home\.dhd\opt\bin"
        "$Home\.dhd\opt\powershell\bin"
        "$Home\opt\bin"

        '7-zip'
        'Amazon\AWSCLI'
        'ConEmu'
        'ConEmu\ConEmu'
        'Docker\Docker\Resources\bin'
        'Docker\Docker\Resources\qemu-img'
        'Docker\Docker\Resources'
        'Git\cmd'
        'GNU\GnuPG'
        'Graphviz*\bin'
        'Kdiff3'
        'LLVM\bin'
        'Microsoft Visual Studio*\VC\bin'
        'Microsoft VS Code\bin\code'
        'MSBuild\*\Bin'
        'NUnit*\bin'
        'OpenSSH'
        'OpenSSL\bin'
        'Oracle\VirtualBox'
        'Powershell\*'
        'Nmap'
        'PuTTY'
        'Sublime Text 3'
        'WordNet\2.1\bin'
        'ZeroTier\One'
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
    Set-FileAssociation .el txtfile
    Set-FileAssociation .nfo txtfile "text/plain"
    Set-FileAssociation .mdwn txtfile "text/plain"
    Set-FileAssociation .markdown txtfile "text/plain"
    Set-FileAssociation .md txtfile "text/plain"
    Set-FileAssociation .text txtfile "text/plain"
    Set-FileAssociation .mkd txtfile "text/plain"
}

# The closest I can get to my .b() bash function is dot-sourcing this function: `. p`
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1"
}

function reinit {
    Setup-SystemPath
    Setup-Environment
}
