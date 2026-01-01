#!/usr/bin/env bash

i=1
for file in ./*.$1; do
    stripped="${file:$2}"
    # stripped="${file#*-}"
    newname=$(printf "%02d. %s" "$i" "$stripped")
    # echo "$newname"
    mv -- "$file" "$newname"
    ((i++))
done
