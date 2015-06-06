#!/bin/bash

for w in $(echo "0.01 0.05 0.1 0.5 1.0"); do 
    for W in $(echo "0.01 0.005 0.001"); do 
        for n in $(echo "0.05 0.01 0.005 0.001"); do 
            for N in $(echo "0.001 0.00001"); do 
                ./dist/build/Myslenie/Myslenie Compression -w $w -W $W -n $n -N $N -e 5;
            done
        done
    done
done

#for w in $(echo "0.01 0.05 0.1 0.5 1.0"); do 
#    for W in $(echo "0.01 0.005 0.001"); do 
#        for n in $(echo "500.0 200.0 100.0 50.0 10.0 1.0 0.5 0.1"); do 
#            for N in $(echo "0.1 0.001"); do 
#                ./dist/build/Myslenie/Myslenie Compression -w $w -W $W -n $n -N $N -e 5;
#            done
#        done
#    done
#done
