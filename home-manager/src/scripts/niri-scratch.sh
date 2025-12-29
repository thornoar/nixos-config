#!/usr/bin/env bash

# scratch_wsp_id="$(niri msg -j workspaces | jq ".[] | select(.name == \"$1\").id")"
scratch_win_id="$(niri msg -j windows | jq ".[] | select(.title == \"$1\").id")"

if [ -z "$scratch_win_id" ]; then
    echo "$scratch_win_id"
    exit 0
fi

# cur_wsp_id="$(niri msg -j workspaces | jq '.[] | select(.is_active == true).id')"
cur_win_id="$(niri msg -j windows | jq '.[] | select(.is_focused == true).id')"

if [[ "$cur_win_id" != "$scratch_win_id" ]]; then
    # if [[ "$cur_wsp_id" != "$scratch_wsp_id" ]]; then
    #     niri msg action focus-workspace --id "$1"
    # fi
    niri msg action focus-window --id "$scratch_win_id"
else
    niri msg action focus-workspace-previous
fi
