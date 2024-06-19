#!/usr/bin/env bash

input="$1"
line=""
if [ ! -z $2 ]; then
    input=$2
    line=$1
fi

pid="$PPID"

while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
    pid="$(ps j $pid | awk 'NR>1 {print $1}')"
    if [[ "$pid" == "0" ]]; then
        nvim $input
        exit 0
    fi
done

nvim --server /run/user/1000/nvim.$pid.pipe --remote $input
if [ ! -z $line ]; then
    nvim --server /run/user/1000/nvim.$pid.pipe --remote-send ":$line<CR>"
fi
nvim --server /run/user/1000/nvim.$pid.pipe --remote-ui
