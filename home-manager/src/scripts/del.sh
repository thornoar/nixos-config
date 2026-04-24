#!/usr/bin/env bash
touch "$1"
mv "$1" "$HOME/media/trash/$(basename "$1")"
