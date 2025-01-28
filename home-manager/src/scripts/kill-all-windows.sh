#!/usr/bin/env bash

pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$(hyprctl -j activeworkspace | jq ".id"))" | jq ".pid")

for pid in $pids; do
    kill "$pid"
done
