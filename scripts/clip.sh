#!/usr/bin/env bash


# arg=$(strip $1)

# clip_util=""
if [ "$XDG_SESSION_TYPE" == "wayland" ]; then
    function strip () {
        arg="$1"
        return "${arg::-1}"
    }

    wl-clip "$(strip "$1")"
else
    echo "$1" | xclip -r -selection c
fi
