#!/usr/bin/env bash

file=""
line=""
target=""
while getopts "f:l:t:" option; do
    case "$option" in
        f) file="$OPTARG" ;;
        l) line="$OPTARG" ;;
        t) target="$OPTARG" ;;
        *) printf "\e[1;31merror:\e[0m Invalid option: %s.\n" "$option"; exit 1 ;;
    esac
done

pid="$PPID"

if [ -n "$target" ]; then
    if [ ! -e "/run/user/1000/nvim.$target.pipe" ]; then
        if [ -n "$line" ] && [ -n "$file" ]; then
            nvim +$line "$file"
        else
            nvim "$file"
        fi
        exit 0
    else
        pid="$target"
    fi
else
    while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
        pid=$(ps j "$pid" | awk 'NR>1 {print $1}')
        if [[ "$pid" == "0" ]]; then
            if [ -n "$line" ] && [ -n "$file" ]; then
                nvim +$line "$file"
            else
                nvim "$file"
            fi
            exit 0
        fi
    done
fi

if [ -n "$file" ]; then
    nvim --server "/run/user/1000/nvim.$pid.pipe" --remote "$(readlink -f "$file")"
    if [ -n "$line" ]; then
        nohup nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-send ":$line<CR>" > /dev/null 2>&1 &
    fi
fi
nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-ui
