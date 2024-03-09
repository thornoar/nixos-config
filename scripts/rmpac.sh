#!/bin/sh
printf "\e[35m| Unistalling Package \"%s\"|\e[0m\n" "$1"
if grep -q $1 "$NIXOS_CONFIG/home-packages"; then
    printf "\e[34m> Removing \"$1\" from package list...\e[0m\n"
    awk "!/$1/" "$NIXOS_CONFIG/home-packages" > temp && mv temp "$NIXOS_CONFIG/home-packages"
    srb
else
	printf "\e[34m> Package already removed from configuration. Run \"sudo nixos-rebuild switch\" to uninstall. Quitting...\"\e[0m\n"
	exit 0
fi
