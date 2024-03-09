#!/bin/sh
printf "\e[35m| Collect Garbage |\e[0m\n"
printf "\e[34m> Collecting on the user level...\e[0m\n"
nix-collect-garbage --delete-old
printf "\e[34m> Collecting on the root level...\e[0m\n"
sudo nix-collect-garbage --delete-old
