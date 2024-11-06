#!/usr/bin/env bash

while read line; do
    if [ "$XDG_SESSION_TYPE" == "wayland" ]; then
        wl-copy "$line"
    else
        echo "$line" | xclip -r -selection c
    fi
done
