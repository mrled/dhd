# Windows Workstation

Use DSC to set up a new Windows workstation.

Ultimately, this will replace my `magic.ps1` and `boxstarter.ps1` scripts.

## Exploring new systems

When exploring new systems, these might be helpful:

    # List all installed programs
    Get-ItemProperty HKLM:\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall* | sort -property DisplayName | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-Table -AutoSize

    # List all store-installed programs
    Get-AppxPackage | sort -property Name | Select-Object Name, PackageFullName, Version | Format-Table -AutoSize

## See also

- https://gist.github.com/jessfraz/7c319b046daa101a4aaef937a20ff41f
- https://gist.github.com/NickCraver/7ebf9efbfd0c3eab72e9

## TO DO

 -  Make installable via a one-liner!
 -  What to do about reboots?
     -  On first application of my config, I will probably usually want to reboot when prompted and then start configuration again
     -  On subsequent applications of my config, I will probably not want to reboot, as I'll be in the middle of something
 -  Schedule config to apply periodically
     -  Maybe just a scheduled task that runs it?
 -  Look at BoxStarter stuff to see if there's anything we could pull in:
    https://github.com/mwrock/boxstarter/tree/master/Boxstarter.WinConfig
 -  Consider uninstalling default crapware.
     -  I was considering adding this to old BoxStarter config:
        -  `Get-AppxPackage *Autodesk* | Remove-AppxPackage`
        -  `Get-AppxPackage *BubbleWitch* | Remove-AppxPackage`
        -  `Get-AppxPackage king.com.CandyCrush* | Remove-AppxPackage`
        -  `Get-AppxPackage *Facebook* | Remove-AppxPackage`
        -  `Get-AppxPackage *Keeper* | Remove-AppxPackage`
        -  `Get-AppxPackage *MarchofEmpires* | Remove-AppxPackage`
        -  `Get-AppxPackage *Minecraft* | Remove-AppxPackage`
        -  `Get-AppxPackage *Netflix* | Remove-AppxPackage`
        -  `Get-AppxPackage *Plex* | Remove-AppxPackage`
        -  `Get-AppxPackage *Twitter* | Remove-AppxPackage`
 -  Ensure Windows Update + Microsoft Update is working as expected.
     -  From old BoxStarter config:
        -  `Enable-MicrosoftUpdate`
        -  `Install-WindowsUpdate -acceptEula`
 -  Install Keybase
 -  Customize Start Menu
     -  Don't override any existing customizations
 -  Customize Task Bar
     -  Don't override any existing customizations
 -  Set some halfway decent desktop background
     -  Don't override an existing desktop background, if I set it myself
 -  Metapad customization
     -  Associate metapad with text files
     -  Uses registry by default, but if metapad.ini is in same directory as metapad.exe it'll use that instead
     -  Which means I'll need to install it to somewhere in $USERPROFILE and then symlink its config file to dhd
 -  Firefox configuration
     -  Enable sync?
     -  about:config settings are NOT synced even if sync is enabled... how to deal with those?
 -  Chrome configuration
     -  ENable sync?
     -  Check whether under-the-hood Chrome changes are synced
