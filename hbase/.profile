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
*/bin/sh*)
# this is because MINGW doesn't properly change its $SHELL variable to bash, even when it is        
    if [ `/bin/uname -o` == "Msys" ]; then
	    if [ -f $HOME/.bashrc -a -r $HOME/.bashrc ]; then
		    . $HOME/.bashrc
	    fi
    fi
	;;
esac

