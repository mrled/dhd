function Send-Notification {
    # We use start-job so that the function can return right away, but also sleep for $seconds
    # before removing the icon from the systray. $objNotifyIcon.ShowBaloonTip() returns immediately
    # and the icon remains even after $seconds, so I needed a way to sleep, but I didn't want it
    # to lock my PS session while it did so. Anyway.
    $sb = {
        param(
            [parameter(mandatory=$true)][string]$message,
            [string]$title="Powershell Notification",
            [ValidateSet("Info","Warning","Error")][string]$icon="Info",
            [int32]$seconds=10
        )

        [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")
        $objNotifyIcon = New-Object System.Windows.Forms.NotifyIcon 
        #systray icon - make this customizable too? It is required but that path doesn't look universal.
        $objNotifyIcon.Icon = "C:\Windows\Installer\{3156336D-8E44-3671-A6FE-AE51D3D6564E}\Icon_app.ico"
        
        $objNotifyIcon.BalloonTipIcon = $icon  #in-balloon icon
        $objNotifyIcon.BalloonTipText = $message
        $objNotifyIcon.BalloonTipTitle = $title
        
        $objNotifyIcon.Visible = $True 
        $objNotifyIcon.ShowBalloonTip($seconds * 1000)
        start-sleep $seconds
        $objNotifyIcon.Visible = $False
        $objNotifyIcon = $null
    }
    $job = start-job -scriptblock $sb -argumentlist @args 

    #return $job #useful for debugging
}

# original version from <http://www.techmumbojumblog.com/?p=39>
# I changed it so it uses invoke-command rather than WMI for remoting
# this means it works only w/ PowerShell 2.0 I think
# neither WMI nor PS remoting enabled by default, but I always enable PS
# remoting on my domains anyway. It does limit it to PS 2.0 though (no XP?)
function Get-InstalledPrograms ($computer = 'localhost') {
    $programs_installed = @{};
    $win32_product = @(invoke-command -computername $computer -scriptblock {get-wmiobject -class 'Win32_Product'});
    foreach ($product in $win32_product) {
        $name = $product.Name;
        $version = $product.Version;
        if ($name -ne $null) {
            $programs_installed.$name = $version;
        }
    }
    return $programs_installed;
}

function Create-Shortcut {
    param(
        [parameter(Mandatory=$true)] [string]$filename,
        [parameter(Mandatory=$true)] [string]$target,
        [string]$arguments = $null,
        [alias("f")] [switch]$force
    )
    if (-not $filename.tolower().endswith(".lnk")) {
        # required, or you'll get an error message and fail. 
        $filename = "$filename.lnk"
    }
    if ((test-path $filename) -and (-not $force.ispresent)) {
        # I don't think I care to check if there's a non-link file named .lnk that we're going to overwrite
        write-error ("Filename $filename already exists; use the -f argument to overwrite.")
        return $null
    }
    $wshshell = New-Object -ComObject WScript.Shell
    $lnk = $wshshell.CreateShortcut($filename)
    $lnk.TargetPath = "$target"
    $lnk.Arguments = "$arguments" #it's ok if this is $null
    $lnk.save()
    return $lnk
}

$startmenu="$env:appdata\Microsoft\Windows\Start Menu"

# seperating file/dir hard/soft links, because they're different in windows
# i wanted to autodetect the target so you didn't have to care, but then you 
# couldn't make hard/soft links that point to a nonexistent file. 
# note that this does not apply to shortcuts
# also, softlinks require admin privs (...wtf)
# finally, note that you have to `remove-item -recurse -force` to delete a junction
# and that this does in fact ONLY delete the hardlink not the target, or the files in the target.
# future ideas: http://stackoverflow.com/questions/2311105/test-in-powershell-code-if-a-folder-is-a-junction-point
function Create-Link {
    param(
        [Parameter(ParameterSetName='shortcut',Mandatory=$true)] [alias("c")] [switch]$shortcut, 
        [Parameter(ParameterSetName='shortcut')] [string]$arguments = $null, 
        #[Parameter(ParameterSetName='shortcut')] [alias("f")] [switch]$force, 
        [Parameter(ParameterSetName='fhardlink',Mandatory=$true)] [alias("h")] [switch]$fhardlink, 
        [Parameter(ParameterSetName='fsoftlink',Mandatory=$true)] [alias("s")] [switch]$fsoftlink, 
        [Parameter(ParameterSetName='dhardlink',Mandatory=$true)] [alias("j")] [switch]$dhardlink, 
        [Parameter(ParameterSetName='dsoftlink',Mandatory=$true)] [alias("d")] [switch]$dsoftlink, 
        [Parameter(Mandatory=$true)] [string]$target,
        [Parameter(Mandatory=$true)] [string]$source
    )
    if (test-path $source) {
        write-error "Filename $source already exists." #cannot overwrite - what if it's not a link?
        return $null
    }
    switch ($pscmdlet.parametersetname) {
        "shortcut" { 
            $a = @{filename = $source
                   target = $target
                   arguments = $arguments
                   force = $force 
            }
            create-shortcut @a 
        }
        "fhardlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink /h $source $target" -wait -NoNewWindow
        }
        "fsoftlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink $source $target" -verb "runAs" -wait
        }
        "dhardlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink /j $source $target" -NoNewWindow -wait
        }
        "dsoftlink" {
            start-process "cmd.exe" -ArgumentList "/c mklink /d $source $target" -verb "runAs" -wait
        }
    }
}
function ln {
    param(
        [Parameter(ParameterSetName='type',Mandatory=$true)] [switch]$s, 

        [string]$trg,
        [string]$src
    )
    if ($f.ispresent) {
        create-shortcut -filename $src -target $trg -force
    }
    else {
        create-shortcut -filename $src -target $trg
    }
}

# note that PS automatically maps certificates to the drive 'cert:'. 
# To import a cert to the local machine's trusted root certs: 
# Import-X509Certificate -path C:\whatever.cer -rootstore LocalMachine -certstore AuthRoot
function Import-X509Certificate {
    param(
        [parameter(required=$true)] [string] $Path,
        [string] $RootStore = "CurrentUser",
        [string] $CertStore = "My"
    )
 
    $pfx = new-object System.Security.Cryptography.X509Certificates.X509Certificate2
    $pfx.import($Path)

    $store = new-object System.Security.Cryptography.X509Certificates.X509Store($CertStore, $RootStore)
    $store.open('MaxAllowed')
    $store.add($pfx)
    $store.close()
}

function EfsEncrypt-File {
    param (
        [alias("r")] [switch]$recurse
    )
    foreach ($a in $args) {
        foreach ($f in get-childitem $a) {
            $f.Encrypt()
        }
    }
}

function reimport-module {
    param([parameter(mandatory=$true)] [string] $moduleName)
    $module = get-module $moduleName
    if ($module) {
        write-host "Module is imported. Removing and re-adding."
        remove-module $moduleName
        import-module $module.path
    }
    else {
        write-host "Module was not imported. Trying to add module $modulename..."
        import-module $modulename
    }
}
set-alias reimport reimport-module

# todo : could mimic set-variable options as closely aspossible
function Set-EnvironmentVariable {
    param(
        [parameter(mandatory=$true)] [string] $Name,
        [string] $Value = "",
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = "Process"
    )
    foreach ($target in $TargetLocation) {
        [Environment]::SetEnvironmentVariable($Name, $Value, $target)
    }
}
set-alias setenv Set-EnvironmentVariable
# TODO: when getting multiple targets, it outputs an array of strings for the value
#       honestly not sure what to do here. %PATH% is *concatenated*, but others are 
#       *replaced* when there's a user and a machine one. ?????
function Get-EnvironmentVariable {
    param(
        [parameter(mandatory=$true)] [string] $Name,
        [validateset("Machine","User","Process")] [string[]] $TargetLocation = "Process"
    )
    $out = ""
    foreach ($target in $TargetLocation) {
        ([Environment]::GetEnvironmentVariable($Name, $target))
    }
}
set-alias getenv Get-EnvironmentVariable

function Get-SystemPath {
    ($env:path).split(";")
}

# http://social.msdn.microsoft.com/Forums/vstudio/en-US/630ed1d9-73f1-4cc0-bc84-04f29cffc13b/
function Set-FileAssociation {
    param(
        [parameter(mandatory=$true)] [string] $extension,
        [parameter(mandatory=$true)] [string] $association,
        [string] $contentType,
        [validateset("User","Machine")] $location = "User"
    )
    if ($location.tolower() -eq "user") {
        $drive = "HKCU"
    }
    elseif ($location.tolower() -eq "machine") {
        if (-not $SoyAdmin) {
            write-error "Cannot set associations for the whole machine unless running as administrator."
            return
        }
        $drive = "HKLM"
    }
    $key = "$($drive):\Software\Classes\$extension"
    if (-not (test-path $key)) {
        new-item -force $key | out-null
    }
    set-itemproperty $key "(default)" $association
    set-itemproperty $key "Content Type" $contentType
}
function Set-AssociationOpenCommand {
    param(
        [parameter(mandatory=$true)] [string] $association,
        [parameter(mandatory=$true)] [string] $command,
        [validateset("User","Machine")] $location = "User"
    )
    if ($location.tolower() -eq "user") {
        $drive = "HKCU"
    }
    elseif ($location.tolower() -eq "machine") {
        if (-not $SoyAdmin) {
            write-error "Cannot set associations for the whole machine unless running as administrator."
            return
        }
        $drive = "HKLM"
    }
    $key = "$($drive):\Software\Classes\$association\shell\open\command"
    if (-not (test-path $key)) {
        new-item -force $key | out-null
    }
    set-itemproperty $key "(default)" $command
}
