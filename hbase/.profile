# .profile, currently just loads ksh if I am ksh

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
# Your previous /Users/mrled/.profile file was backed up as /Users/mrled/.profile.macports-saved_2009-05-16_at_19:47:43
##

# MacPorts Installer addition on 2009-05-16_at_19:47:43: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.


# MacPorts Installer addition on 2009-05-16_at_19:47:43: adding an appropriate MANPATH variable for use with MacPorts.
export MANPATH=/opt/local/share/man:$MANPATH
# Finished adapting your MANPATH environment variable for use with MacPorts.

