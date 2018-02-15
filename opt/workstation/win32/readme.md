# Windows Workstation

Use DSC to set up a new Windows workstation.

This is a replacement for my short-lived Boxstarter script.

Install via a one-liner like:

    iwr -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/workstation/win32/dscInit.ps1 | iex

- Run from an elevated prompt
- It will prompt for a credential - this should be the credential for your user account, so that DSC can set user-specific registry settings etc

## How this works

You probably don't wanna use this directly unless you are literally me.

That said, it might be interesting to see how it works.

1. I use `dscInit.ps1` in this directory as a front-end, script that can be curlbashed from an elevated Powershell 5.x prompt
2. Manages all work that has to be done before DSC, which I'm trying to keep as minimal as possible
3. It calls the DSC configuration in `dscConfiguration.ps1` in this directory
4. I wrote some custom DSC resources in ../../powershell/modules which are used as well

The goal is something that works like Boxstarter, but is more secure (Boxstarter curlbashes over unencrypted HTTP) and more modular. Over time, I may break out pieces of the DSC configuration into multiple files. Also, some parts of the configuration might be useful on non-workstation machines, so it may be worth breaking them out so they can be composed anywhere.

I also like how DSC contains complexity. DSC resources do have kind of a lot of code in them, but they're a really nice containment/abstraction around that code.

In the future, I am planning to apply my configurations on a schedule, to manage drift.

## Exploring new systems

When exploring new systems, particularly new Windows versions or OEM crapware, it may be useful to see installed programs so that they can be removed.

    # List all installed programs
    Get-ItemProperty HKLM:\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall* | sort -property DisplayName | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-Table -AutoSize

    # List all store-installed programs
    Get-AppxPackage | sort -property Name | Select-Object Name, PackageFullName, Version | Format-Table -AutoSize

## See also

- https://gist.github.com/jessfraz/7c319b046daa101a4aaef937a20ff41f
- https://gist.github.com/NickCraver/7ebf9efbfd0c3eab72e9

## TO DO

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
