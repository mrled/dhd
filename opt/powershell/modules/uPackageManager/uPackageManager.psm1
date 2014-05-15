# Put something in your PATH by creating a .bat file there that calls it (ewwwwww)
# Was gonna use shortcuts for this but guess what, you have to call them with the .lnk at the end. 
# fucking lol. 

# This just gets the latest version according to the default Python install scheme
# which gets you C:\Python27, C:\Python33, etc
$matchingPythonDirs = get-item C:\python* | sort 
if ($matchingPythonDirs.count -gt 0) {
    $pythondir = $matchingPythonDirs[-1]
    $pythonexe = "$pythondir\python.exe"
}

function Install-Exe {
    param(
        [parameter(Mandatory=$true)] [string] $exe,
        [string] [alias("name")] $installname,
        [string] [validateset("exe", "python")] $exetype = "exe",
        [switch] $force,
        [switch] $IncludeUninstallers,
        [string] $installdir="$Home\opt\win32bin",
        [string] $pythonpath
    )
    if ($exetype -eq "python") {
        if ($pythonpath) {
            if (-not (test-path $pythonpath)) { 
                write-error "Passed -pythonpath but there is no such executable '$pythonpath'."
                return
            }
            $pe = $pythonpath
        }
        elseif ($pythonexe) {
            if (-not (test-path $pythonexe)) {
                write-error "Using the pythonexe global variable, but there is no such executable '$pythonexe'."
                return
            }
            $pe = $pythonexe
        }
        else {
            write-error "Tried to install a python script, but there is no python.exe to be found."
            return
        }
    }
    if (-not (test-path $exe)) {
        write-error ("No such file: '$exe'.")
        return
    }
    $fsio = get-item $exe
    $justname = (($fsio.name -replace ("\.lnk$","")) -replace ("\.exe$",""))
    $fullpath = $fsio.fullname

    # Ignore uninstallers
    if (-not ($IncludeUninstallers.ispresent)) {
        $uninstNames = @("unins000.exe", "uninstall.exe")
        if ($uninstNames.contains($fsio.name)) {
            write-host "Tried to run Install-Exe on an uninstaller executable called $($fsio.fullname), but -IncludeUninstallers switch was not present." -foreground Yellow
            return
        }
    }
                

    if ($installname) {
        $scpath = "$installdir\$installname.bat"
    }
    else { 
        $scpath = "$installdir\$justname.bat"
    }
    
    if (test-path $scpath) { 
        if ($force.ispresent) {
            rm $scpath
        }
        else {
            write-error ("Shortcut path '$scpath' exists, and '-force' was not supplied.")
            return
        }
    }

    mkdir -force $installdir > $null # just in case we're on a new box

    write-host "Installing $fullpath to $scpath..."

    if ($exetype -eq "exe") { 
        # Here we are writing out a .bat file (in ASCII, not the default UTF-8).
        # ascii because: http://bytes.com/topic/net/answers/546745-ef-bb-bf-prepended
        "@ECHO OFF" | out-file $scpath -encoding "ASCII" -append
        "`"$fullpath`" %*" | out-file $scpath -encoding "ASCII" -append
    }
    elseif ($exetype -eq "python") {
        "@ECHO OFF" | out-file $scpath -encoding "ASCII" -append
        "$pe `"$fullpath`" %*" | out-file $scpath -encoding "ASCII" -append
    }
}

export-modulemember install-exe