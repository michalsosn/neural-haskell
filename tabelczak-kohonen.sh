#!/bin/bash

for w in $(echo "0.01 0.05 0.1 0.5 1.0"); do 
    for W in $(echo "0.01 0.005 0.001"); do 
        for n in $(echo "0.05 0.01 0.005 0.001"); do 
            for N in $(echo "0.001 0.00001"); do 
                echo "$w $W $n $N " $(./dist/build/Myslenie/Myslenie Kohonen -w $w -W $W -n $n -N $N -e 20);
            done
        done
    done
done
