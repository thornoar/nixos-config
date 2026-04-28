#!/usr/bin/env bash

for file in ./**/*."$1"; do
    stripped="${file%.*}"
    ffmpeg -i "$file" "$stripped.$2"
done
