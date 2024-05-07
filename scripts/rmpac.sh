#!/usr/bin/env bash
if grep -q "$1" "$NIXOS_CONFIG/home-ramak/packages.txt"; then
    printf "\e[34m> Removing \"\e[35m%s\e[34m\" from package list...\e[0m\n" "$1"
    awk "!/$1/" "$NIXOS_CONFIG/home-ramak/packages.txt" > temp && mv temp "$NIXOS_CONFIG/home-ramak/packages.txt"
    printf "\e[34m> Rebuild your system for the package to be installed.\e[0m\n"
else
	printf "\e[34m> Package already removed from configuration. Run \"sudo nixos-rebuild switch\" to uninstall.\e[0m\n"
	exit 0
fi
