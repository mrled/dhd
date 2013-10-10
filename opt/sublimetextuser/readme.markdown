# Sublime Text 2 shit

I used to put my whole Sublime Text 2 folder in here, but it meant for a lot of bullshit changes all the time. 

I'm now just syncing the Packages/User directory. This means I don't get packages sync'd; I have to install Package Control on each new system. However, Package Control adds all installed packages to its user preferences, so when it gets installed, it should auto install the packages I put there. Also, if I ever install a package outside of Package Control, it won't get sync'd at all. 

I do, however, .gitignore all the Package Control -related files in this directory, other than `Preferences.sublime-settings`.

## The symlinks

In CMD (*must* be run elevated):

	cd "%APPDATA%\Sublime Text 2\Packages"
	mklink /d User C:\Users\Micah\.dhd\opt\sublimetextuser

On Mac OS X:

	cd '~/Library/Application Support/Sublime Text 2/Packages'
	ln -s ~/.dhd/opt/sublimetextuser User

