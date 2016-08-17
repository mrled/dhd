# .profile
    
if   [ ! "x$BASH_VERSION" = x ]; then #bash
    if [ -f $HOME/.dhd/hbase/.bashrc -a -r $HOME/.dhd/hbase/.bashrc ]; then
        . $HOME/.dhd/hbase/.bashrc
    fi
fi
