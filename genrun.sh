#!/bin/bash
depth=1000
num=10
file=minimincaml.ne

# Generate a temp file 
for ((i = 0 ; i < $num; i++)); do 
    nearleyc ./generators/$file -o ./generators/$file.js
    nearley-unparse -n 1 ./generators/$file.js -o tmp.ml;
    echo "=========="
    cat tmp.ml;
    echo;
    echo "==========";
    ./main.byte tmp.ml tmp.ml || rm tmp.ml; exit -2;
    rm tmp.ml;
    rm ./generators/$file.js;
    done