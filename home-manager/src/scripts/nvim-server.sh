#!/usr/bin/env bash

verbose=false
list=false
restart=false
clean=false
while getopts "vlrcb" option; do
    case "$option" in
    v) verbose=true ;;
    l) list=true ;;
    r) restart=true ;;
    c) clean=true ;;
    *) printf "\e[1;31merror:\e[0m Invalid option: %s.\n" "$option"; exit 1 ;;
    esac
done

function start {
    pipe="$1"
    nohup nvim --listen "$pipe" --headless > /dev/null 2>&1 0< /dev/null &
    # nohup nvim --server "$pipe" --remote-send ":silent bdelete<CR>" > /dev/null 2>&1 0< /dev/null
    if [ "$verbose" = true ]; then
        printf "| \e[34mCreated NVIM server at pipe \e[35m%s\e[34m.\e[0m\n" "$pipe"
    fi
}

function stop {
    pipe="$1"
    nohup nvim --server "$pipe" --remote-send "<esc>:wqa<CR>" > /dev/null 2>&1 0< /dev/null
    nohup rm "$pipe" > /dev/null 2>&1 0< /dev/null
}

if [ "$clean" = true ]; then
    # nohup killall nvim > /dev/null 2>&1
    # sleep 0.5
    for pipe in /run/user/1000/nvim.*.pipe; do
        stop "$pipe"
    done
    exit 0
fi

if [ "$restart" = true ]; then
    if [ "$verbose" = true ]; then
        printf "| \e[34mRestarting neovim servers...\e[0m\n"
    fi

    pipelist=()
    for file in /run/user/1000/nvim.*.pipe; do
        if [ -e "$file" ]; then
            pipelist+=("$file")
        fi
    done

    if [ "$verbose" = true ]; then
        printf "| \e[34mStopping NVIM servers at the following pipes:\e[0m\n"
        # for pipe in "${pipelist[@]}"; do
        #     printf "  %s\n" "$pipe"
        # done
    fi

    for pipe in "${pipelist[@]}"; do
        stop "$pipe"
        if [ "$verbose" = true ]; then
            printf "  \e[35m%s\e[0m\n" "$pipe"
        fi
    done

    if [ "$verbose" = true ]; then
        printf "| \e[34mWaiting...\e[0m\n"
    fi
    sleep 0.5

    for pipe in "${pipelist[@]}"; do
        if ps -p "$(echo "$pipe" | sed -r "s/.*\.([0-9]+)\..*/\1/g")" > /dev/null
        then
            start "$pipe"
        else
            if [ "$verbose" = true ]; then
                printf "  pipe \e[35m%s\e[0m no longer active, it will be forgotten\n" "$pipe" # ]]
            fi
        fi
    done
    exit 0
fi

if [ "$list" = true ]; then
    if [ "$verbose" = true ]; then
        printf "| \e[34mListing active NVIM servers:\e[0m\n" # ]]
    fi
    for file in /run/user/1000/nvim.*.pipe; do
        if [ -e "$file" ]; then
            echo "  $file"
        fi
    done
    exit 0
fi

pipe="/run/user/1000/nvim.$PPID.pipe"
nohup rm "$pipe" > /dev/null 2>&1 0< /dev/null &
start "$pipe"
