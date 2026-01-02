#!/usr/bin/env bash

ext="$1"
num="$2"
# rem="$3"

for file in *."$ext"; do
    pref="${file:0:$num}"
    fst="${file:$num}"
    snd="${fst#* - }"
    # snd="${fst#"$rem"}"
    # echo "$pref$snd"
    mv "$file" "$pref$snd"
done
