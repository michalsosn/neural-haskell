#!/bin/bash

for w in $(echo "0.01 0.05 0.1 0.5 1.0"); do 
    for W in $(echo "0.01 0.005 0.001"); do 
        (echo "$w-$W"; wcat "$1" | grep "^$w $W" | cut -d ' ' -f 7) | tr '\n' '&'  | cut -d '&' -f 1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25 | sed 's/&$/\n/'
        echo "\\\\ \\hline"
    done
done
