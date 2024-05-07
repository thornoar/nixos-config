#!/usr/bin/env bash
setxkbmap -layout us,ru,de
xkb-switch -s "$1"
