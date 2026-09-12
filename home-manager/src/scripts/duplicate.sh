#!/usr/bin/env bash

if [ "$1" = "-r" ]; then
    niri msg action focus-monitor-left
    niri msg action close-window
    niri msg action focus-monitor eDP-1
    exit 0
fi

niri msg action focus-monitor-left
wl-mirror eDP-1 &
sleep 1
niri msg action fullscreen-window
niri msg action focus-monitor eDP-1
