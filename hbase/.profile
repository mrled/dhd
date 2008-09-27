# .profile, currently just loads ksh if I am ksh

case $SHELL in 
ksh)
        if [ -f $HOME/.kshrc -a -r $HOME/.kshrc ]; then
            ENV=$HOME/.kshrc
            export ENV
        fi
        ;;
esac
