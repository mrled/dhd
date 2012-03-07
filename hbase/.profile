# .profile

if   [ ! "x$BASH_VERSION" = x ]; then #bash
    if [ -f $HOME/.bashrc -a -r $HOME/.bashrc ]; then
        . $HOME/.bashrc
    fi
elif [ ! "x$KSH_VERSION" = x ]; then #ksh
    if [ -f $HOME/.kshrc -a -r $HOME/.kshrc ]; then
        ENV=$HOME/.kshrc
        export ENV
    fi
elif [ `/bin/uname -o` = "Msys" ]; then
    # this is because MINGW doesn't properly change its $SHELL variable to bash, even when it is        
    if [ -f $HOME/.bashrc -a -r $HOME/.bashrc ]; then
        . $HOME/.bashrc
    fi
fi
