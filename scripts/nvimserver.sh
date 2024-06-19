#!/usr/bin/env bash

pid="$PPID"

nohup rm /run/user/1000/nvim.$pid.pipe > /dev/null 2>&1 0< /dev/null &
nohup nvim --listen /run/user/1000/nvim.$pid.pipe --headless > /dev/null 2>&1 0< /dev/null &
