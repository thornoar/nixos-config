#!/usr/bin/env bash

scratch_wsp="scratch"
cur_wsp="$(niri msg -j workspaces | jq '.[] | select(.is_active == true)' | jq '.name')"

if [ "$cur_wsp" != "$scratch_wsp" ]; then
    niri msg action focus-workspace "$scratch_wsp"
else
    niri msg action focus-workspace-previous
fi
