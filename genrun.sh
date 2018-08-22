#!/bin/bash
depth=1000
num=10

# Generate a temp file 
for ((i = 0 ; i < $num; i++)); do 
    nearley-unparse -n 1 -d $depth examplegen/minimincaml.js -o tmp.ml;
    echo "=========="
    cat tmp.ml;
    echo
    echo "==========";
    ./main.byte tmp.ml tmp.ml
    rm tmp.ml; done