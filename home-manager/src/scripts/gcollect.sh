#!/usr/bin/env bash
printf "\e[1;34m#\e[0m Collecting garbage on the user level...\n" # ]]
nix-collect-garbage --delete-old
printf "\e[1;34m#\e[0m Collecting garbage on the root level...\n" # ]]
sudo nix-collect-garbage --delete-old
printf "\e[1;34m#\e[0m Deleting boot entries...\n" # ]]
nix-collect-garbage -d
