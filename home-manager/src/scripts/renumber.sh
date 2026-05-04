#!/usr/bin/env bash

num=$3
if [ -z "$num" ]; then
    num="2"
fi

i=1
for file in ./*."$1"; do
    nfile="$(basename "$file")"
    stripped="${nfile:$2}"
    # stripped="${nfile#*.}"
    newname=$(printf "%0${num}d. %s" "$i" "$stripped")
    # echo "$newname"
    mv -- "$file" "$newname"
    ((i++))
done
