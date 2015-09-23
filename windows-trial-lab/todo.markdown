- what the FUCK is going on in vagrant-ssh.bat
- better windows update mechanism imo
- would like to use the -tag in the name for the vagrant box too, but that requires parameterizing both the packerfile and the vagrantfile template :/ not sure what to do about this 
- store passwords securely for shit and/or generate them on the fly
- test `lab2 -action VagrantUp -baseConfigName windows_81_x86 -tag PreLunchTest` tomorrow afternoon - will it have 89 days remaining? or 90? basically, was it activated at boot or nah? 
- need to audit **all** of the scripts ../scripts actually
- it seems like the vbox tools aren't getting installed? why not?
- enable clipboard and drag&drop in my Vagrantfile - though NOT for throwaway VMs that might be insecure! 
- Seems like I'm not actually defragging it in my compact.bat? 
- Fucking vbox guest tools aren't installing, god I hate batch scripts

Dumb shit I figured out about how fucking bad Packer is while I got to do this

- The shell, windows-shell, and powershell provisioners are VERY finicky. I canNOT make them work reliably
- Best thing to do is upload a script with a "file" provisioner and run it with a "windows-shell" provisioner that has one inline command
- ps you can't upload directories for some reason. just individual files. one. by. fucking. one. 

