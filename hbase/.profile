# .profile

case $SHELL in 
*ksh)
        if [ -f $HOME/.kshrc -a -r $HOME/.kshrc ]; then
            ENV=$HOME/.kshrc
            export ENV
        fi
        ;;
*bash*)
	if [ -f $HOME/.bashrc -a -r $HOME/.bashrc ]; then
		. $HOME/.bashrc
	fi
	;;
esac



##
# Your previous /Users/mrled/.profile file was backed up as /Users/mrled/.profile.macports-saved_2010-09-25_at_15:16:42
##

# MacPorts Installer addition on 2010-09-25_at_15:16:42: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

