# Windows Workstation

Use DSC to set up a new Windows workstation.

Ultimately, this will replace my `magic.ps1` and `boxstarter.ps1` scripts.

## TO DO

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
