#!/usr/bin/env bash

shopt -s globstar

for file in ./**/*."$1"; do
    stripped="${file%.*}"
    ffmpeg -i "$file" "$stripped.$2" $3
done
