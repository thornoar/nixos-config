#!/usr/bin/env bash

raw=0

function printUsage {
    printf "usage: hyprland-hack [ close-special | move-all-windows | kill-all-windows ]\n"
    printf "                     [ -h|--help | -r|--raw ]\n"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -r|--raw) raw=1; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

cmd="$1"

if [[ "close-special" =~ ^"$cmd" ]]; then
    active=$(hyprctl -j monitors | jq --raw-output '.[] | select(.focused==true).specialWorkspace.name | split(":") | if length > 1 then .[1] else "" end')
    if [[ ${#active} -gt 0 ]]; then
        hyprctl dispatch togglespecialworkspace "$active"
    fi
elif [[ "kill-all-windows" =~ ^"$cmd" ]]; then
    pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$(hyprctl -j activeworkspace | jq ".id"))" | jq ".pid")
    for pid in $pids; do
        kill "$pid"
    done
elif [[ "move-all-windows" =~ ^"$cmd" ]]; then
    cur_workspace="$(hyprctl -j activeworkspace | jq ".id")"
    cur_pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$cur_workspace)" | jq ".pid")
    to_workspace="$2"
    hyprctl dispatch workspace "$to_workspace"
    to_pids=$(hyprctl -j clients | jq --raw-output ".[] | select(.workspace.id==$(hyprctl -j activeworkspace | jq ".id"))" | jq ".pid")
    for pid in $cur_pids; do
        hyprctl dispatch movetoworkspacesilent "$to_workspace,pid:$pid"
    done
    for pid in $to_pids; do
        hyprctl dispatch movetoworkspacesilent "$cur_workspace,pid:$pid"
    done
    hyprctl dispatch workspace "$to_workspace"
else
    if [ "$raw" -eq 0 ]; then printf "! Unknown command: \e[33m%s\e[0m.\n" "$cmd"; fi
    exit 1
fi
