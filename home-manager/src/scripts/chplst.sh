#!/usr/bin/env bash

if test -f /tmp/timestamp; then
    prev="$(cat /tmp/timestamp)"
    now="$(date "+%s")"
    diff=$((now - prev))
    echo $diff
    if test $diff -lt 60; then
        exit 0
    fi
fi

date "+%s" > /tmp/timestamp
rmpc clear
ls "$MEDIA/music/Playlists" | shuf -n 1 | sed 's/\..*//g'
rmpc load "$(ls "$MEDIA/music/Playlists" | shuf -n 1 | sed 's/\..*//g')"
rmpc single off
rmpc play
