#!/usr/bin/env bash

cmd="$1"

if [ "$cmd" = "scratch" ]; then
    cur_wsp_name="$(niri msg -j workspaces | jq -r '.[] | select(.is_active == true).name')"
    if [ "$cur_wsp_name" = "$2" ]; then
        niri msg action focus-workspace "$(cat /tmp/wsphook)"
    else
        if [[ "$cur_wsp_name" =~ [0-9] ]]; then
            echo hi
            echo "$cur_wsp_name" > /tmp/wsphook
        fi
        niri msg action focus-workspace "$2"
    fi
elif [ "$cmd" = "shrink" ]; then
    cur_wsp_id="$(niri msg -j workspaces | jq '.[] | select(.is_active == true).id')"
    for win_id in $(niri msg -j windows | jq ".[] | select(.workspace_id == $cur_wsp_id).id"); do
        niri msg action set-window-width --id "$win_id" "50%"
    done
    niri msg action focus-column-first
elif [ "$cmd" = "seq" ]; then
    for action in "${@:2}"; do
        niri msg action "$action"
    done
elif [ "$cmd" = "next-wsp" ]; then
    cur_wsp_name="$(niri msg -j workspaces | jq -r '.[] | select(.is_active == true).name')"
    if [[ "$cur_wsp_name" =~ [0-4] ]]; then
        niri msg action focus-workspace-down
    fi
elif [ "$cmd" = "prev-wsp" ]; then
    cur_wsp_name="$(niri msg -j workspaces | jq -r '.[] | select(.is_active == true).name')"
    if [[ "$cur_wsp_name" =~ [1-5] ]]; then
        niri msg action focus-workspace-up
    fi
elif [ "$cmd" = "close-all" ]; then
    cur_wsp_id="$(niri msg -j workspaces | jq '.[] | select(.is_active == true).id')"
    for win_id in $(niri msg -j windows | jq ".[] | select(.workspace_id == $cur_wsp_id).id"); do
        niri msg action close-window --id "$win_id"
    done
fi
