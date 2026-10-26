#!/usr/bin/env bash

if test -f /tmp/timestamp; then
    prev="$(cat /tmp/timestamp)"
    now="$(date "+%s")"
    diff=$((now - prev))
    if test $diff -lt 300; then
        exit 0
    fi
fi

date "+%s" > /tmp/timestamp
rmpc clear
rmpc load "$(ls "$MEDIA/music/Playlists" | shuf -n 1 | sed 's/\..*//g')"
rmpc single off
rmpc play
