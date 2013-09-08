# Sublime Text 2 shit

I used to put my whole Sublime Text 2 folder in here, but it meant for a lot of bullshit changes all the time. 

I'm now just syncing the Packages/User directory. This means I don't get packages sync'd; I have to install ST2 and then Package Control and then each package I want to use on each system I want to use, instead of having it all done for me. 

(Note that this means I am .gitignore'ing all the Package Control -related stuff from this directory.)

## The symlinks

In CMD (*must* be run elevated):

	cd "%APPDATA%\Sublime Text 2\Packages"
	mklink /d User C:\Users\Micah\.dhd\opt\sublimetextuser

On Mac OS X:

	cd '~/Library/Application Support/Sublime Text 2/Packages'
	ln -s ~/.dhd/opt/sublimetextuser User

