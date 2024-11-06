#!/usr/bin/env bash
height=""
if [ -n "$1" ]; then
    height=" --height $1"
fi
broot --cmd ":pt$height"
