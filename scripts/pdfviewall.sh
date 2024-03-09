#!/bin/sh
for file in "$PWD"/*; do
    if [[ $file == *.pdf ]]
    then
        nohup zathura "$file"&
    fi
done;
