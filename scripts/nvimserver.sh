#!/usr/bin/env bash

pid="$PPID"

nohup rm /run/user/1000/nvim.$pid.pipe > /dev/null 2>&1 0< /dev/null &
nohup nvim --listen /run/user/1000/nvim.$pid.pipe --headless > /dev/null 2>&1 0< /dev/null &

# if [[ "$1" == "--restart" ]]; then
#     killall nvim
#     nohup rm /run/user/1000/nvim.server.started > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.1.pipe > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.2.pipe > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.3.pipe > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.4.pipe > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.5.pipe > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.1.occupied > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.2.occupied > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.3.occupied > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.4.occupied > /dev/null 2>&1
#     nohup rm /run/user/1000/nvim.server.5.occupied > /dev/null 2>&1
# elif [ -f "/run/user/1000/nvim.server.started" ]; then
#     exit 0
# fi
#
# touch /run/user/1000/nvim.server.started
#
# nohup nvim --listen /run/user/1000/nvim.server.1.pipe --headless > /dev/null 2>&1  0< /dev/null &!
# nohup nvim --listen /run/user/1000/nvim.server.2.pipe --headless > /dev/null 2>&1 0< /dev/null &!
# nohup nvim --listen /run/user/1000/nvim.server.3.pipe --headless > /dev/null 2>&1 0< /dev/null &!
# nohup nvim --listen /run/user/1000/nvim.server.4.pipe --headless > /dev/null 2>&1 0< /dev/null &!
# nohup nvim --listen /run/user/1000/nvim.server.5.pipe --headless > /dev/null 2>&1 0< /dev/null &!
# # if [ ! -f "/run/user/1000/nvim.server.1.pipe" ]; then
# #     # nohup killall nvim > /dev/null 2>&1 0< /dev/null &
# # fi
# # if [ ! -f "/run/user/1000/nvim.server.2.pipe" ]; then
# # fi
# # if [ ! -f "/run/user/1000/nvim.server.3.pipe" ]; then
# # fi
# # if [ ! -f "/run/user/1000/nvim.server.4.pipe" ]; then
# # fi
# # if [ ! -f "/run/user/1000/nvim.server.5.pipe" ]; then
# # fi
