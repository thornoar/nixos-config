#!/usr/bin/env bash
touch "$1"
mv "$1" "$HOME/media/trash/$(date +%d-%M-%Y-%s)-$(basename "$1")"



