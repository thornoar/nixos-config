#!/usr/bin/env bash

if [ -z "$XDG_RUNTIME_DIR" ]; then
	export XDG_RUNTIME_DIR="/run/user/$(id -u)"
fi

export STATUS_FILE="$XDG_RUNTIME_DIR/transparentcursor.status"

set_transparent() {
	printf "true" > "$STATUS_FILE"
    hyprctl setcursor transparent 32
}

set_normal() {
	printf "false" > "$STATUS_FILE"
    hyprctl setcursor Adwaita 32
}

if ! [ -f "$STATUS_FILE" ]; then
	set_transparent
else
	if [ "$(cat "$STATUS_FILE")" = "true" ]; then
		set_normal
	elif [ "$(cat "$STATUS_FILE")" = "false" ]; then
		set_transparent
	fi
fi
