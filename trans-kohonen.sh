#!/bin/bash

for w in $(echo "0.01 0.05 0.1 0.5 1.0"); do 
    for W in $(echo "0.01 0.005 0.001"); do 
        (echo "$w-$W"; wcat "$1" | grep "^$w $W" | cut -d ' ' -f 6) | tr '\n' '&' | sed 's/&$/\n/'
        echo "\\\\ \\hline"
    done
done
