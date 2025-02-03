#!/usr/bin/env bash
if grep -q "$1" "$NIXOS_CONFIG/home-manager/src/packages.txt"; then
    printf "\e[1;34m#\e[0m Removing \"\e[35m%s\e[0m\" from package list...\n" "$1" #]]]]
    awk "!/$1/" "$NIXOS_CONFIG/home-manager/src/packages.txt" > temp && mv temp "$NIXOS_CONFIG/home-manager/src/packages.txt"
    printf "\e[1;34m#\e[0m Rebuild the system to uninstall.\n" #]]
else
	printf "\e[1;31m#\e[0m Package not present in configuration.\n" #]]
	exit 0
fi
