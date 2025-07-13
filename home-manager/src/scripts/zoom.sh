#!/usr/bin/env bash
amt="$1"
cur=$(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2}');
new=$(echo "$cur + ($cur + ($amt) >= 1.0)*($amt)" | bc)
hyprctl -q keyword cursor:zoom_factor $new
# 1.1 + (1.1 + (-0.1) >= 1.0)*(-0.1)
