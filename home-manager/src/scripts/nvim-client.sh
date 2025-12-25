#!/usr/bin/env bash

file=""
line=""
target=""
showhelp=0
while getopts "f:l:t:h" option; do
    case "$option" in # (((((
        f) file="$OPTARG" ;;
        l) line="$OPTARG" ;;
        t) target="$OPTARG" ;;
        h) showhelp=1 ;;
        *) exit 1 ;;
    esac
done

if [ $showhelp == 1 ]; then
    printf "usage: nvim-client [OPTIONS]\n"
    printf "  -f FILE    the file to edit\n"
    printf "  -l NUM     the line number to jump to\n"
    printf "  -t PIPE    the target pipe to find the server\n"
    printf "  -h         show this help message\n"
    exit 0
fi

pid="$PPID"

if [ -n "$target" ]; then
    if [ ! -e "/run/user/1000/nvim.$target.pipe" ]; then
        if [ -n "$line" ] && [ -n "$file" ]; then
            $EDITOR "+$line" "$file"
        else
            $EDITOR "$file"
        fi
        exit 0
    else
        pid="$target"
    fi
else
    while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
        pid=$(ps j "$pid" | awk 'NR>1 {print $1}')
        if [[ "$pid" -eq 0 ]]; then
            if [ -n "$line" ] && [ -n "$file" ]; then
                $EDITOR "+$line" "$file"
            else
                $EDITOR "$file"
            fi
            exit 0
        fi
    done
fi

if [ -n "$file" ]; then
    nohup nvim --server "/run/user/1000/nvim.$pid.pipe" --remote "$(readlink -f "$file")" > /dev/null 2>&1
    if [ -n "$line" ]; then
        nohup nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-send ":$line<CR>" > /dev/null 2>&1 &
    fi
fi

nvim --server "/run/user/1000/nvim.$pid.pipe" --remote-ui
