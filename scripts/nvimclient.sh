#!/usr/bin/env bash

input="$1"
line=""
if [ ! -z $2 ]; then
    input=$2
    line=$1
fi

server=""

if [ -f "/run/user/1000/nvim.server.started" ]; then
    if [ ! -f "/run/user/1000/nvim.server.1.occupied" ]; then
        server="1"
    elif [ ! -f "/run/user/1000/nvim.server.2.occupied" ]; then
        server="2"
    elif [ ! -f "/run/user/1000/nvim.server.3.occupied" ]; then
        server="3"
    elif [ ! -f "/run/user/1000/nvim.server.4.occupied" ]; then
        server="4"
    elif [ ! -f "/run/user/1000/nvim.server.5.occupied" ]; then
        server="5"
    fi
fi

if [ ! -z $server ]; then
    touch /run/user/1000/nvim.server.$server.occupied
    nvim --server /run/user/1000/nvim.server.$server.pipe --remote $input
    if [ ! -z $line ]; then
        nvim --server /run/user/1000/nvim.server.$server.pipe --remote-send ":$line<CR>"
    fi
    nvim --server /run/user/1000/nvim.server.$server.pipe --remote-ui
    rm /run/user/1000/nvim.server.$server.occupied
else
    if [ ! -z $line ]; then
        nvim +$line $input
    else
        nvim $input
    fi
fi
