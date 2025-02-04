#!/usr/bin/env bash

file=""
line=""
target=""
query=0
while getopts "f:l:t:q" option; do
    case "$option" in # (((((
        f) file="$OPTARG" ;;
        l) line="$OPTARG" ;;
        t) target="$OPTARG" ;;
        q) query=1 ;;
        *) printf "\e[1;31m#\e[0m Invalid option: %s.\n" "$option"; exit 1 ;;
    esac
done

pid="$PPID"

if [ -n "$target" ]; then
    if [ ! -e "/run/user/1000/nvim.$target.pipe" ]; then
        if [ $query == 1 ]; then printf "No server found on target %s\n" "$target"; exit 0; fi
        if [ -n "$line" ] && [ -n "$file" ]; then
            nvim +$line "$file"
        else
            nvim "$file"
        fi
        exit 0
    else
        if [ $query == 1 ]; then printf "Found server on target:\n  %s\n" "/run/user/1000/nvim.$target.pipe"; exit 0; fi
        pid="$target"
    fi
else
    while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
        pid=$(ps j "$pid" | awk 'NR>1 {print $1}')
        if [[ "$pid" -eq 0 ]]; then
            if [ $query == 1 ]; then printf "No server found.\n"; exit 1; fi
            if [ -n "$line" ] && [ -n "$file" ]; then
                nvim +$line "$file"
            else
                nvim "$file"
            fi
            exit 0
        fi
    done
fi

if [ $query == 1 ]; then printf "Found server on target:\n  %s\n" "/run/user/1000/nvim.$pid.pipe"; exit 0; fi

if [ -n "$file" ]; then
    nohup nvim --server "/run/user/1000/nvim.$pid.pipe" --remote "$(readlink -f "$file")" > /dev/null 2>&1
    if [ -n "$line" ]; then
        nohup nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-send ":$line<CR>" > /dev/null 2>&1 &
    fi
fi
nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-ui
