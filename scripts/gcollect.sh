#!/usr/bin/env bash
printf "| \e[34mCollecting garbage on the user level...\e[0m\n" # ]]
nix-collect-garbage --delete-old
printf "| \e[34mCollecting garbage on the root level...\e[0m\n" # ]]
sudo nix-collect-garbage --delete-old
printf "| \e[34mDeleting boot entries...\e[0m\n" # ]]
nix-collect-garbage -d
