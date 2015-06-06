#!/bin/bash

for e in $(echo "5 10 20 30 40 50"); do 
    echo "$e " $(./dist/build/Myslenie/Myslenie NeuralGas -e $e);
done
