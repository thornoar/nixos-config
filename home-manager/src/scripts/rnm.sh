#!/usr/bin/env bash

file="$1"
newname="$2"

# echo "$(dirname "$file")/$newname"
# echo "$file"
# echo
mv "$file" "$(dirname "$file")/$newname"
