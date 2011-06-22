#!/bin/bash
for ffile in ~/.dhd/doc/*.txt; do
    noext=${ffile%.txt}
    strfile $ffile $noext.dat > /dev/null
done

