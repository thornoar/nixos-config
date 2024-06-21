#!/usr/bin/env bash

verbose=false
list=false
restart=false
clean=false
while getopts "vlrc" option; do
    case "$option" in
    v) verbose=true ;;
    l) list=true ;;
    r) restart=true ;;
    c) clean=true ;;
    *) printf "\e[1;31merror:\e[0m Invalid option: %s.\n" "$option"; exit 1 ;;
    esac
done

if [ "$clean" = true ]; then
    nohup killall nvim > /dev/null 2>&1
    sleep 0.2

    for file in /run/user/1000/nvim.*.pipe; do
        nohup rm "$file" > /dev/null 2>&1
    done

    exit 0
fi
if [ "$restart" = true ]; then
    if [ "$verbose" = true ]; then
        printf "\e[34m> Restarting neovim servers...\e[0m\n"
    fi

    pipelist=()
    for file in /run/user/1000/nvim.*.pipe; do
        if [ -e "$file" ]; then
            pipelist+=("$file")
        fi
    done

    nohup killall nvim > /dev/null 2>&1
    sleep 0.2

    for pipe in "${pipelist[@]}"; do
        # echo "$pipe" | sed -r "s/.*\.([0-9]+)\..*/\1/g"
        if ps -p "$(echo "$pipe" | sed -r "s/.*\.([0-9]+)\..*/\1/g")" > /dev/null
        then
            nohup rm "$pipe" > /dev/null 2>&1
            nohup nvim --listen "$pipe" --headless > /dev/null 2>&1 0< /dev/null &
        else
            if [ "$verbose" = true ]; then
                printf "  pipe \e[35m%s\e[0m no longer active, it will be forgotten\n" "$pipe"
            fi
        fi
    done
    exit 0
fi

if [ "$list" = true ]; then
    if [ "$verbose" = true ]; then
        printf "\e[34m> Listing active NVIM servers:\e[0m\n"
    fi
    for file in /run/user/1000/nvim.*.pipe; do
        if [ -e "$file" ]; then
            echo "  $file"
        fi
    done
else
    pid="$PPID"
    if [ "$verbose" = true ]; then
        printf "\e[34m> Creating NVIM server at PID \e[35m%s\e[0m\n" "$pid"
    fi
    nohup rm "/run/user/1000/nvim.$pid.pipe" > /dev/null 2>&1 0< /dev/null &
    nohup nvim --listen "/run/user/1000/nvim.$pid.pipe" --headless > /dev/null 2>&1 0< /dev/null &
fi
