#!/bin/tcsh 

setenv EDU  "~/doc/edu/2007-1(spring)"

cd $EDU
foreach e ( .[1-9] )
    cd $e
    if ( -fx .mirror ) then
        ./.mirror
    endif
    cd ..
    end

