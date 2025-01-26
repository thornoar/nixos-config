#!/usr/bin/env bash

to_workspace=$1
pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$(hyprctl -j activeworkspace | jq ".id"))" | jq ".pid")

for pid in $pids; do
    hyprctl dispatch movetoworkspace "$to_workspace,pid:$pid"
done
