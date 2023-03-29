#!/bin/bash

##########################################
# @file test.sh                          #
# @author Boris Štrbák (xstrba05)        #
#                                        #
# Run tests flp22-fun with option given  #
# by first argument.                     #
# For example test.sh -o will test       #
# flp22-fun -o                           #
##########################################

echo Running tests for switch "$1"

for inFile in ./tests/*.in
do
    outFile="${inFile/.in/.out}"
    if [[ -f $outFile ]]; then
        echo ""
        echo Running test "$inFile"
        diff "${outFile}" <(./flp22-fun -"$1" "${inFile}") && echo "Test OK"
    fi;
done