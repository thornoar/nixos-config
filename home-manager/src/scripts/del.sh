#!/usr/bin/env bash
mv "$1" "$HOME/media/trash/$(date +%d-%M-%Y)-$(basename "$1")"
