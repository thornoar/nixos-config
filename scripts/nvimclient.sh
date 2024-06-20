#!/usr/bin/env bash

# if [ -n "$2" ]; then
#     input=$2
#     line=$1
# fi

input="$1"
line=""
verbose=false
while getopts "vi:l:" flag; do
    case "$flag" in
        v)
            verbose=true
        ;;
        i)
            input="$OPTARG"
        ;;
        l)
            line="$OPTARG"
        ;;
        *)
            printf "flag %s is unrecognized.\n" "$flag"
        ;;
    esac
done

pid="$PPID"

while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
    pid=$(ps j "$pid" | awk 'NR>1 {print $1}')
    if [[ "$pid" == "0" ]]; then
        if [ "$verbose" = true ]; then
            printf "\e[32mwarning: Failed to find NVIM server PID. Will start regular NVIM."
        fi
        nvim "$input"
        exit 0
    fi
done

if [ "$verbose" = true ]; then
    printf "\e[34m> Found NVIM server at PID: \e[35m%s\e[0m\n" "$pid"
fi

if [ -n "$input" ]; then
    nvim --server "/run/user/1000/nvim.$pid.pipe" --remote "$input"
    if [ -n "$line" ]; then
        nohup nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-send ":silent $line<CR>" > /dev/null 2>&1 &
    fi
fi
nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-ui
