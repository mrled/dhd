windows-trial-lab: scripts for building one or more machines from Windows trial ISOs

## Layout and script purpose

- marionettist/windows-trial-lab/
	- scripts/
		- windeploy-marionettist/
			- windeploy-marionettist.psm1
			- (etc)
		- autounattend-postinstall.ps1      # run from Autounattend.xml, contains hardcoded values
		- packer-postinstall.ps1            # run by a packer provisioner, contains hardcoded values
		- download-windowsupdates.ps1       # run on schedule, should use a config file somewhere
		- build-updatedwindowsisos.ps1		# run on schedule, should use a config file somewhere
		- build-vagrantboxes.ps1     		# run on schedule, should use a config file somewhere
		- buildlab.ps1                      # ?? maybe? controls the whole flow of everything? I at least need something that talks to Packer
		- (any other scripts)

