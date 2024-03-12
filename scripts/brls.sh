#!/usr/bin/env bash
height=""
if [ -z "$1" ]; then
    height=" --height $1"
fi

echo "broot --cmd \":pt\"$height"

broot --cmd ":pt"$height
