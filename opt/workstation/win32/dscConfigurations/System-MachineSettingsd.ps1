<#
.synopsis
Configure machine settings. Requires admin privileges.
#>

Configuration MachineSettingsConfig {
    param(
        [string[]] $ComputerName = "localhost"
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration
    Import-DscResource -ModuleName cMrlPathLikeEnvVar
    Import-DscResource -ModuleName cMrlPathLikeEnvVarSet

    Node $ComputerName {

        # Only applicable on Windows Server... TODO: restrict this to only Windows Server
        Registry "Do Not Open Server Manager At Login" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\ServerManager"
            ValueName = "DoNotOpenServerManagerAtLogon"
            ValueData = 1
            ValueType = "DWord"
        }

        # Set CapsLock to Ctrl
        # From original in pure Powershell:
        #   $layoutBytes = ([byte[]] @(0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x1d,0x00,0x3a,0x00,0x00,0x00,0x00,0x00))
        #   Set-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout" -Name "Scancode Map" -Type Binary -Value $layoutBytes
        # See also: https://serverfault.com/questions/865450/dsc-syntax-for-binary-registry-key
        Registry "Set CAPSLOCK to CTRL" {
            Key = "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout"
            ValueName = "Scancode Map"
            ValueData = "0000000000000000020000001d003a0000000000"
            ValueType = "Binary"
        }

        # Disabling telemetry requires a reboot to take effect
        Registry "Disable Telemetry: Registry Keys" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\DataCollection"
            ValueName = "AllowTelemetry"
            ValueData = 0
            ValueType = "DWord"
        }
        Service "Disable Telemetry: Connected User Experiences and Telemetry Service" {
            Name = "DiagTrack"
            StartupType = "Disabled"
            State = "Stopped"
        }
        Service "Disable Telemetry: WAP Push Message Routing Service" {
            Name = "Dmwappushservice"
            StartupType = "Disabled"
            State = "Stopped"
        }

        Registry "Disable Automatic Installation of Bullshit Apps that Microsoft Gets Paid To Push, Seriously?" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\CloudContent"
            ValueName = "DisableWindowsConsumerFeatures"
            ValueData = 1
            ValueType = "DWord"
        }

        Registry "Disable WiFi Sense HotSpot Sharing" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\PolicyManager\default\WiFi\AllowWiFiHotSpotReporting"
            ValueName = "Value"
            ValueData = 0
            ValueType = "DWord"
        }
        Registry "Disable WiFi Sense Shared HotSpot Auto-Connect" {
            Key = "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\PolicyManager\default\WiFi\AllowAutoConnectToWiFiSenseHotspots"
            ValueName = "Value"
            ValueData = 0
            ValueType = "DWord"
        }

        cMrlPathLikeEnvVarSet "PrependPathEnvironmentVariables" {
            Name = "PATH"
            Location = @(
                "${env:ChocolateyInstall}\bin"
                "${env:SystemDrive}\ProgramData\Chocolatey\bin"

                "${env:SystemDrive}\Perl64"
                "${env:SystemDrive}\Python3*"
                "${env:SystemDrive}\Python3*\Scripts"
                "${env:SystemDrive}\Python2*"
                "${env:SystemDrive}\Python2*\Scripts"
                "${env:SystemDrive}\Tools\Go\bin"
                "${env:SystemDrive}\Tools\mingw64\bin"
                "${env:SystemDrive}\Tools\Python3*"
                "${env:SystemDrive}\Tools\Python3*\Scripts"
                "${env:SystemDrive}\Tools\Ruby*\bin"

                "${env:ProgramFiles}\7-zip"
                "${env:ProgramFiles}\Amazon\AWSCLI"
                "${env:ProgramFiles}\ConEmu"
                "${env:ProgramFiles}\ConEmu\ConEmu"
                "${env:ProgramFiles}\Docker\Docker\Resources\bin"
                "${env:ProgramFiles}\Docker\Docker\Resources\qemu-img"
                "${env:ProgramFiles}\Docker\Docker\Resources"
                "${env:ProgramFiles}\Git\cmd"
                "${env:ProgramFiles}\GNU\GnuPG"
                "${env:ProgramFiles}\GnuWin32\bin"
                "${env:ProgramFiles}\Graphviz*\bin"
                "${env:ProgramFiles}\Kdiff3"
                "${env:ProgramFiles}\LLVM\bin"
                "${env:ProgramFiles}\Microsoft Visual Studio*\VC\bin"
                "${env:ProgramFiles}\Microsoft VS Code\bin"
                "${env:ProgramFiles}\MSBuild\*\Bin"
                "${env:ProgramFiles}\NUnit*\bin"
                "${env:ProgramFiles}\OpenSSH"
                "${env:ProgramFiles}\OpenSSL\bin"
                "${env:ProgramFiles}\Oracle\VirtualBox"
                "${env:ProgramFiles}\Powershell\*"
                "${env:ProgramFiles}\Nmap"
                "${env:ProgramFiles}\PuTTY"
                "${env:ProgramFiles}\Sublime Text 3"
                "${env:ProgramFiles}\WordNet\2.1\bin"
                "${env:ProgramFiles}\ZeroTier\One"
                "${env:ProgramFiles}\vim\vim*"
                "${env:ProgramFiles}\vim\vim*\macros"

                "${env:ProgramFiles(x86)}\GNU\GnuPG"
                "${env:ProgramFiles(x86)}\GnuWin32\bin"
                "${env:ProgramFiles(x86)}\MSBuild\*\Bin"
                "${env:ProgramFiles(x86)}\vim\vim*"
                "${env:ProgramFiles(x86)}\vim\vim*\macros"
            )
            Ensure = "Present"
            InsertionMode = "Prepend"
            OnlyIfExists = $true
        }
    }
}
