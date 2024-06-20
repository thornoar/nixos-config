#!/usr/bin/env bash

printf "\e[34m> Restarting neovim servers...\e[0m\n"

pipelist=()

for file in /run/user/1000/nvim.*.pipe; do
    pipelist+=("$file")
done

nohup killall nvim > /dev/null 2>&1

sleep 0.2

for pipe in "${pipelist[@]}"; do
    nohup rm "$pipe" > /dev/null 2>&1
    nohup nvim --listen "$pipe" --headless > /dev/null 2>&1 0< /dev/null &
done
