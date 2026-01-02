#!/usr/bin/env bash

i=1
for file in ./*.$1; do
    nfile="$(basename "$file")"
    stripped="${nfile:$2}"
    # stripped="${nfile#*.}"
    newname=$(printf "%02d. %s" "$i" "$stripped")
    # echo "$newname"
    mv -- "$file" "$newname"
    ((i++))
done
