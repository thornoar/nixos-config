#!/usr/bin/env bash
if grep -q "$1" "$NIXOS_CONFIG/home-manager/src/packages.txt"; then
    printf "| \e[34mRemoving \"\e[35m%s\e[34m\" from package list...\e[0m\n" "$1" #]]]]
    awk "!/$1/" "$NIXOS_CONFIG/home-manager/src/packages.txt" > temp && mv temp "$NIXOS_CONFIG/home-manager/src/packages.txt"
    printf "| \e[34mRebuild the system to uninstall.\e[0m\n" #]]
else
	printf "| \e[34mPackage already removed from configuration. Rebuild the system to uninstall.\e[0m\n" #]]
	exit 0
fi
