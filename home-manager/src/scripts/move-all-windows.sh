#!/usr/bin/env bash

cur_workspace="$(hyprctl -j activeworkspace | jq ".id")"
cur_pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$cur_workspace)" | jq ".pid")

to_workspace=$1
hyprctl dispatch workspace "$to_workspace"
to_pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$(hyprctl -j activeworkspace | jq ".id"))" | jq ".pid")

for pid in $cur_pids; do
    hyprctl dispatch movetoworkspacesilent "$to_workspace,pid:$pid"
done
for pid in $to_pids; do
    hyprctl dispatch movetoworkspacesilent "$cur_workspace,pid:$pid"
done
