#!/bin/sh
printf "\e[35m| Install Packages |\e[0m\n"
if grep -q "$1" "$NIXOS_CONFIG/home-packages"; then
	printf "\e[34m> Package already listed in configuration. Run \"sudo nixos-rebuild switch\" to install. Quitting...\e[0m\n"
	exit 0
fi
nix search nixpkgs#"$1" || exit 1
printf "\e[34m> Prepending \"\e[0m\e[35m%s\e[0m\e[34m\" to package list...\e[0m\n" "$1"
echo "$1" >> "$NIXOS_CONFIG/home-packages"
srb -i
