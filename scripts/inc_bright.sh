#!/usr/bin/env bash
brightfile=""
if [ "$PCTYPE" = "laptop" ]; then
    brightfile="/sys/class/backlight/intel_backlight/brightness"
    bc <<< "$1 + $(cat $brightfile)" > "$brightfile"
fi
