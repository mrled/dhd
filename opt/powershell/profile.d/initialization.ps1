# Initialization stuff doesn't need to happen very often, and it might be slow ish maybe

function Get-PythonDir {
    # Get the latest version installed (assuming the default Python install path)
    $matchingPythonDirs = get-item C:\python* | sort 
    if ($matchingPythonDirs.count -gt 0) {
        return $matchingPythonDirs[-1].fullname
    }
}
$PythonDir = Get-PythonDir
function Get-VimDir {
    if (test-path "${env:programfiles(x86)}\vim") {
        $possibleVimDirs += @((ls "${env:programfiles(x86)}\vim\vim??").fullname | sort-object -descending)[0]
    }
    if (test-path "${env:programfiles}\vim") {
        $possibleVimDirs += @((ls "${env:programfiles}\vim\vim??").fullname | sort-object -descending)[0]
    }
    foreach ($vd in $possibleVimDirs) {
        if ($vd) {
            return $vd
        }
    }
}
$VimDir = Get-VimDir

function Setup-SystemPath {
    $possiblePaths = @(
        "${env:ChocolateyInstall}\bin"
        "C:\Chocolatey\bin"
        "C:\ProgramData\Chocolatey\bin"
        "$home\.dhd\opt\win32bin"
        "$home\.dhd\opt\powershell\bin"
        "$home\opt\win32bin"
        "$home\opt\Console2"
        "$home\opt\SysinternalsSuite"
        "$home\opt\mupdf"
        "C:\opt\strawberry\perl\bin"
        "C:\opt\GnuWin32\bin"
        "C:\opt\GnuWin32\sbin"
        "C:\opt\local\bin"
        "C:\opt\svn\bin"
        "C:\opt\SysinternalsSuite"
        "C:\opt\nirsoft64"
        "C:\opt\nirsoft_package"
        "C:\opt\Console2"
        "C:\opt\UnxUtils\bin"
        "C:\opt\UnxUtils\usr\local\wbin"
        "C:\opt\sqlite"
        "C:\Program Files (x86)\Git\cmd"
        "C:\Program Files\PuTTY"
        "C:\Program Files\7-Zip"
        "C:\Program Files (x86)\7-Zip"
        "C:\Program Files (x86)\PuTTY"
        "C:\Program Files\Windows SDKs\Windows\v7.0\Bin"
        "C:\Program Files\NSIS"
        "C:\Program Files (x86)\NSIS"
        "C:\Program Files\Nmap"
        "C:\Program Files (x86)\Nmap"
        "C:\Program Files (x86)\VMware\VMware Virtual Disk Development Kit\bin"
        "C:\Program Files\VMware\VMware Virtual Disk Development Kit\bin"
    )

    $azureSDKs = "$env:ProgramFiles\Microsoft SDKs\Windows Azure\.NET SDK"
    if (test-path $azureSDKs) {
        $possiblePaths += "$((gci $azureSDKs[-1]).fullname)\bin"
    }

    if ($pythondir) {
        # PythonDir contains python.exe; Scripts contains stuff installed from Distribute packages; Tools/Scripts contains.py files. 
        $possiblePaths += @($pythondir,"$pythondir\Scripts","$pythondir\Tools\Scripts")
    }
    $possiblePaths += @((getenv path user) -split ';')
    $possiblePaths = $possiblePaths.tolower() | sort -unique

    if ($VimDir) {
        $possiblePaths += @($VimDir,"$VimDir\macros")
    }

    $existingPaths = @()
    foreach ($pp in $possiblePaths) {
        if ($pp -and (test-path $pp)) { 
            write-host "Adding value to system path: $pp"
            $existingPaths += @($pp) 
        }
    }

    # Set the path for *future processes*
    # We only set the User path, not the system path.
    # This won't take effect in the current shell
    setenv -name Path -value "$($existingPaths -join ";")" -targetlocation User

    # Set the path for *the current shell*
    # Since this will replace the path variable completely, we also have to go through the system path
    $existingPaths += @((getenv path machine) -split ';')
    setenv -name Path -value "$($existingPaths -join ";")" -targetlocation Process
}

function Setup-Environment {
    # Term is important for things like less.exe, sometimes
    set-environmentvariable -name "TERM" -value "xterm" -targetlocation user,process

    Set-FileAssociation .el txtfile
    Set-FileAssociation .nfo txtfile "text/plain"
    Set-FileAssociation .mdwn txtfile "text/plain"
    Set-FileAssociation .markdown txtfile "text/plain"
    Set-FileAssociation .md txtfile "text/plain"
    Set-FileAssociation .text txtfile "text/plain"
    Set-FileAssociation .mkd txtfile "text/plain"

    # Python stuff
    $pythondir = Get-PythonDir
    if ($pythondir) {
        $pythonexe = "$pythondir\python.exe"
        Set-FileAssociation .py Python.File
        Set-AssociationOpenCommand Python.File "$pythonexe `"%1`" %*"
        set-environmentvariable -name PYTHONSTARTUP -value "$home\.dhd\hbase\python.profile" -targetlocation user,process
    }
}

function Setup-AdminEnvironment {
    if (Get-PythonDir) {
        if (-not (getenv pathext machine).tolower().contains('.py')) {
            $pathext = getenv pathext machine
            $pathext += ';.PY'
            setenv pathext $pathext machine,process
        }
    }
}

# The closest I can get to my .b() bash function is dot-sourcing this function: `. p`
function p {
    . "$Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1" 
}

function reinit {
    . p
    Setup-SystemPath
    Setup-Environment
    if (-not $SoyAdmin) {
        write-host -foreground Yellow "Could not run Setup-AdminEnvironment because you are not an admin"
    }
    else {
        Setup-AdminEnvironment
    }
}

