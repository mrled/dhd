windows-trial-lab: scripts for building one or more machines from Windows trial ISOs

## Credits

This started as some customizations for [joefitzgerald/packer-windows](https://github.com/joefitzgerald/packer-windows) that got a liiiiiiiittle out of hand. 

These were the *types* of changes I'm trying to make: 

- I rewrote their Windows Update script to be much more readable (imo). Now it has clearly defined functions with parameter blocks, you can set the postinstall step when calling it (rather than hardcoding calling `A:\openssh.ps1`) and you only have to set it once, and all functions MAY read global variables set at the top level but DO NOT write to them. 
- I want to use WinRM rather than OpenSSH
    - As a result of this, I don't copy anything to the host for provisioning, because this is buggy with WinRM. This isn't a big deal though - I just put everything I want to use on the A:\ drive and use the "attach" guest additions mode in Packer
    - I have a much easier time dealing with my provisioners though
- I rewrote lots of their scripts as functions in my Powershell module, and a couple of scripts that call into that module
    - This means that my Autounattend.xml is simpler - I just have one or two postinstall commands in there. The last one must enable WinRM.
    - It also means my packerfile is simpler AND it lets me place comments next to commands - packerfile uses JSON which doesn't allow this for stupid reasons
- I log to Windows Event Log

And these are some specific changes that may impact you

- The original project has [a way to install KB2842230](https://github.com/joefitzgerald/packer-windows/blob/master/scripts/hotfix-KB2842230.bat). I haven't run into this problem, but if I did, I'd have to figure this one out too. I'm not sure but it appears that they have an installer script but not a downloader script - it's unclear whether people are actually using this or not. 
- The original project has [a script that forces all network locations to be private](https://github.com/joefitzgerald/packer-windows/blob/master/scripts/fixnetwork.ps1), which is necessary to enable PS Remoting. I haven't hit a problem that this solved yet, so I don't include it. 
    - The Windows 10 Autounattend.xml also sets the NewNetworkWindowOff registry key, per <https://technet.microsoft.com/en-us/library/gg252535%28v=ws.10%29.aspx>, by doing `cmd.exe /c reg add "HKLM\System\CurrentControlSet\Control\Network\NewNetworkWindowOff"`, before running the fixnetwork.ps1 script.
- I don't have VMware, only VirtualBox, so all the VMware support was removed (since I was rewriting it and couldn't test it, this seemed like the right thing to do)
- I use WinRM rather than OpenSSH
- I don't include installers for puppet/salt/etc
- I had to change the vagrant user's password to something more complex so you could remote in to it

## Layout and script purpose

- marionettist/windows-trial-lab/
    - buildlab.ps1                          # controls the whole flow of everything
	- scripts/
		- windeploy-marionettist/
			- windeploy-marionettist.psm1
			- (etc)
		- autounattend-postinstall.ps1      # run from Autounattend.xml, contains hardcoded values
		- provisioner-postinstall.ps1       # run by a packer provisioner, contains hardcoded values
        - win-updates.ps1                   # run from autounattend-postinstall if desired, reboots system repeatedly
        - enable-winrm.ps1                  # run from autounattend-postinstall
    - packer/ 
        - (folders for each version of Windows)

## To do

buildlab.ps1 improvements: 

- would like to use the -tag in the name for the vagrant box too, but that requires parameterizing both the packerfile and the vagrantfile template :/ not sure what to do about this
- I have a concept of "packer basename" and "tag" in buildlab. Extend this to also have "architecture" and "flavor" (or something - to capture Server Standard vs Core vs Datacenter etc) 

packer/vagrant/postinstall improvements:

- store passwords securely for shit and/or generate them on the fly
- use client certs for WinRM: https://msdn.microsoft.com/en-us/library/aa384295%28v=vs.85%29.aspx ?? only if packer/vagrant can support it tho
- would be great if I didn't have duplicated Autounattend.xml files everywhere - can I templatize this?  
- in Autounattend.xml, we turn off UAC. (That's the `<EnableLUA>false</EnableLUA>` setting.) Is this really required? Or was it only required for using shitty SSH? 

vagrant provisioners

- decide on a systems management system. DSC seems like maybe the most natural option.
- pull down git, conemu, my dhd repo 
- configure launch bar
- configure taskbar 

other improvements

- I really wish I had a way to slipstream updates into ISOs so the first Windows Update run is just getting recent stuff. There are 150+ updates for Win 8.1 at first boot, and these take a few hours to install. Ughhhh.

upstream improvements

- It's possible that the original project might be interested in some of the stuff I've done, particularly the Windows Update work, and maybe even my postinstall module. Clean up the code and submit it to them and see what they think. 

## Whines

- The shell, windows-shell, and powershell provisioners are VERY finicky. I canNOT make them work reliably. The easiest thing I can figure out how to do is to use a Powershell provisioner to call a file with no arguments over WinRM. lmfao
- However the situation was much improved when I switched to WinRM with the powershell provisioner. That seems to work OK
- I think the problem was that using the shell provisioner with OpenSSH, which provides an emulated POSIX environment of some kind
