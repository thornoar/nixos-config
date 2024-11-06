#!/usr/bin/env bash
if grep -q "$1" "$NIXOS_CONFIG/home-manager/packages.txt"; then
    printf "| \e[34mRemoving \"\e[35m%s\e[34m\" from package list...\e[0m\n" "$1" #]]]]
    awk "!/$1/" "$NIXOS_CONFIG/home-manager/packages.txt" > temp && mv temp "$NIXOS_CONFIG/home-manager/packages.txt"
    printf "| \e[34mRebuild your system for the package to be installed.\e[0m\n" #]]
else
	printf "| \e[34mPackage already removed from configuration. Run \"sudo nixos-rebuild switch\" to uninstall.\e[0m\n" #]]
	exit 0
fi
