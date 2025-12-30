#!/usr/bin/env bash

cmd="$1"

if [ "$cmd" = "scratch" ]; then
    scratch_win_id="$(niri msg -j windows | jq ".[] | select(.title == \"$2-scratch\").id")"
    if [ -z "$scratch_win_id" ]; then
        echo "$scratch_win_id"
        exit 0
    fi
    cur_win_id="$(niri msg -j windows | jq '.[] | select(.is_focused == true).id')"
    if [[ "$cur_win_id" != "$scratch_win_id" ]]; then
        niri msg action focus-window --id "$scratch_win_id"
    else
        niri msg action focus-workspace-previous
    fi
elif [ "$cmd" = "toggle-shrink" ]; then
    cur_wsp_id="$(niri msg -j workspaces | jq '.[] | select(.is_active == true).id')"
    for win_id in $(niri msg -j windows | jq ".[] | select(.workspace_id == $cur_wsp_id).id"); do
        # niri msg action set-window-width --id "$win_id" "proportion 0.5"
        niri msg action switch-preset-window-width --id "$win_id"
    done
    niri msg action focus-column-first
fi
