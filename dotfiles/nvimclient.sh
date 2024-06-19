#!/usr/bin/env bash

date=$(date +%j%H%M%N)

nohup rm /run/user/1000/$date.pipe > /dev/null 2>&1 0< /dev/null &
nohup nvim --listen /run/user/1000/$date.pipe --headless > /dev/null 2>&1 0< /dev/null &

function nvimclient {
    input="$1"
    line=""
    if [ ! -z $2 ]; then
        input=$2
        line=$1
    fi
    nvim --server /run/user/1000/$date.pipe --remote $input
    if [ ! -z $line ]; then
        nvim --server /run/user/1000/$date.pipe --remote-send ":$line<CR>"
    fi
    nvim --server /run/user/1000/$date.pipe --remote-ui
}
