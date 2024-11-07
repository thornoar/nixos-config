#!/usr/bin/env bash

swww img "$(find $MEDIA/wallpapers/$WALLPAPER_DIR -type f | shuf -n 1)" --transition-duration 1 --transition-type right
