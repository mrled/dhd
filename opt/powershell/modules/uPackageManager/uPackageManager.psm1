# Put something in your PATH by creating a .bat file there that calls it (ewwwwww)
# Was gonna use shortcuts for this but guess what, you have to call them with the .lnk at the end. 
# fucking lol. 

<#
.synopsis
"Install" an EXE by creating a batch file linking it somewhere in your PATH
.parameter exe
The EXE to install
.parameter InstallName
The name of the batch file to create. (Defaults to the name of the executable.)
.parameter ScriptType
Indicates the executable is actually a script that must be invoked from an interpreter. The value of this parameter is assumed to be the name of the interpreter, which must be present in the PATH. For example, "-ScriptType Python" assumes that there is an interpreter that can be called as "python" in your PATH. 

This creates a batch file like: 

    @ECHO OFF
    python C:\Path\To\Exe %*
.parameter Force
If the batch file already exists in its location, overwrite it. 
.parameter InstallDir
The location where the batch file will be installed. 
#>
function Install-Exe {
    param(
        [parameter(Mandatory=$true)] [string] [ValidateScript({
            test-path $_
            })] $exe,
        [string] [alias("name")] $installname,
        [string] $ScriptType,
        [switch] $force,
        [string] $installdir="$Home\opt\win32bin"
    )

    $fsio = get-item $exe
    $justname = (($fsio.name -replace ("\.lnk$","")) -replace ("\.exe$",""))
    $fullpath = $fsio.fullname

    # Ignore uninstallers
    $uninstNames = @("unins000.exe", "uninstall.exe")
    if ($uninstNames.contains($fsio.name)) {
        write-host "Tried to run Install-Exe on an uninstaller executable called $($fsio.fullname), but -IncludeUninstallers switch was not present." -foreground Yellow
        return
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
            throw ("Shortcut path '$scpath' exists, and '-force' was not supplied.")
        }
    }

    mkdir -force $installdir > $null # just in case we're on a new box

    write-verbose "Installing $fullpath to $scpath..."

    $batchFileContents = "@ECHO OFF`r`n"
    if ($ScriptType) {
        $batchFileContents += "$ScriptType "
    }
    $batchFileContents += "`"$fullpath`" %*"

    # Here we are writing out a .bat file (in ASCII, not the default UTF-8).
    # ascii because: http://bytes.com/topic/net/answers/546745-ef-bb-bf-prepended
    $batchFileContents | out-file $scpath -encoding "ASCII" -append
}

export-modulemember install-exe