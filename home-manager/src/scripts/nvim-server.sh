#!/usr/bin/env bash

fast=0
verbose=0
delete=0
list=0
rst=0
all=0
showhelp=0
while getopts "fvdlrah" option; do
    case "$option" in
    f) fast=1 ;;
    v) verbose=1 ;;
    d) delete=1 ;;
    l) list=1 ;;
    r) rst=1 ;;
    a) all=1 ;;
    h) showhelp=1 ;;
    *) exit 1 ;;
    esac
done

function start {
    pipe="$1"
    nohup nvim --listen "$pipe" --headless > /dev/null 2>&1 0< /dev/null &
    nohup nvim --server "$pipe" --remote-send ":silent bdelete<CR>" > /dev/null 2>&1 0< /dev/null
    if [ "$verbose" = 1 ]; then
        printf "> Created NVIM server at pipe \e[32m%s\e[34m.\e[0m\n" "$pipe"
    fi
}

if [ $fast == 1 ]; then
    verbose=0
    pipe="/run/user/1000/nvim.$PPID.pipe"
    start "$pipe"
    exit 0
fi

if [ $showhelp == 1 ]; then
    printf "usage: nvim-server [OPTIONS]\n"
    printf "  -f    skip all checks and start the server\n"
    printf "  -v    print log messages\n"
    printf "  -d    delete the currently running server\n"
    printf "  -l    check if there is an active server attached to the current PID\n"
    printf "  -r    restart the server\n"
    printf "  -a    apply an action to all running servers\n"
    printf "  -h    show this help message\n"
    exit 0
fi

function stop {
    pipe="$1"
    nohup nvim --server "$pipe" --remote-send "<esc>:wqa<CR>" > /dev/null 2>&1 0< /dev/null
    nohup rm "$pipe" > /dev/null 2>&1 0< /dev/null
}

if [ $all == 1 ]; then
    if [ $delete == 1 ]; then
        if [ $verbose == 1 ]; then
            printf "> Deleting all active NVIM servers:\n"
        fi
        for pipe in /run/user/1000/nvim.*.pipe; do
            if [ -e "$pipe" ]; then
                if [ $verbose == 1 ]; then
                    printf "  \e[32m%s\e[0m\n" "$pipe" # ]]
                else
                    printf "%s\n" "$pipe"
                fi
                stop "$pipe"
            fi
        done
        exit 0
    elif [ $list == 1 ]; then
        if [ $verbose == 1 ]; then
            printf "> Listing active NVIM servers:\n" # ]]
        fi
        for pipe in /run/user/1000/nvim.*.pipe; do
            if [ -e "$pipe" ]; then
                if [ $verbose == 1 ]; then
                    printf "  \e[32m%s\e[0m\n" "$pipe" # ]]
                else
                    printf "%s\n" "$pipe"
                fi
            fi
        done
        exit 0
    elif [ $rst == 1 ]; then
        pipelist=()
        for file in /run/user/1000/nvim.*.pipe; do
            if [ -e "$file" ]; then
                pipelist+=("$file")
            fi
        done

        if [ $verbose == 1 ]; then
            printf "> Stopping NVIM servers at the following pipes:\n"
        fi

        for pipe in "${pipelist[@]}"; do
            stop "$pipe"
            if [ $verbose == 1 ]; then
                printf "  \e[32m%s\e[0m\n" "$pipe"
            fi
        done

        if [ $verbose == 1 ]; then
            printf "> Waiting...\n"
        fi
        sleep 0.5

        for pipe in "${pipelist[@]}"; do
            if ps -p "$(echo "$pipe" | sed -r "s/.*\.([0-9]+)\..*/\1/g")" > /dev/null
            then
                start "$pipe"
            else
                if [ $verbose == 1 ]; then
                    printf "  pipe \e[32m%s\e[0m no longer active, it will be forgotten\n" "$pipe" # ]]
                fi
            fi
        done
        exit 0
    fi
    if [ $verbose == 1 ]; then
        printf "! No command given.\n"
    fi
    exit 0
fi

if [ $delete == 0 ] && [ $rst == 0 ] && [ $list == 0 ]; then
    old_pid="$PPID"
    pid="$PPID"
    while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
        pid=$(ps j "$pid" | awk 'NR>1 {print $1}')
        if [[ "$pid" -eq 0 ]]; then
            pipe="/run/user/1000/nvim.$old_pid.pipe"
            start "$pipe"
            if [ $verbose == 1 ]; then
                printf "> Started server on pipe \e[32m%s\e[0m\n" "$pipe"
            fi
            exit 0
        fi
    done
    if [ $verbose == 1 ]; then
        printf "! Server already exists.\n"
    fi
    exit 1
fi

pid="$PPID"
while [ ! -e "/run/user/1000/nvim.$pid.pipe" ]; do
    pid=$(ps j "$pid" | awk 'NR>1 {print $1}')
    if [[ "$pid" -eq 0 ]]; then
        if [ $verbose == 1 ]; then
            printf "! No server found.\n"
        fi
        exit 1
    fi
done

pipe="/run/user/1000/nvim.$pid.pipe"
if [ $delete == 1 ]; then
    if [ $verbose == 1 ]; then
        printf "> Deleting server on pipe \e[32m%s\e[0m\n" "$pipe"
    fi
    stop "$pipe"
    exit 0
elif [ $list == 1 ]; then
    if [ $verbose == 1 ]; then
        printf "> Found server on pipe \e[32m%s\e[0m\n" "$pipe"
    else
        printf "%s\n" "$pipe"
    fi
    exit 0
elif [ $rst == 1 ]; then
    if [ $verbose == 1 ]; then
        printf "> Stopping NVIM server at pipe \e[32m%s\e[0m\n" "$pipe"
    fi
    stop "$pipe"
    if [ $verbose == 1 ]; then
        printf "> Waiting...\n"
    fi
    sleep 0.5
    start "$pipe"
fi
