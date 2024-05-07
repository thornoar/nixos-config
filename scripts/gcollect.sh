#!/usr/bin/env bash
printf "\e[34m> Collecting garbage on the user level...\e[0m\n"
nix-collect-garbage --delete-old
printf "\e[34m> Collecting garbage on the root level...\e[0m\n"
sudo nix-collect-garbage --delete-old
printf "\e[34m> Deleting boot entries...\e[0m\n"
nix-collect-garbage -d
