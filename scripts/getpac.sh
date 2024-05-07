#!/usr/bin/env bash
if grep -q "^$1$" "$NIXOS_CONFIG/home-ramak/packages.txt"; then
	printf "\e[34m> Package \"\e[35m%s\e[34m\" already listed in configuration. Run \"sudo nixos-rebuild switch\" to install. Quitting...\e[0m\n" "$1"
	exit 0
fi
nix search nixpkgs#"$1" || exit 1
printf "\e[34m> Appending \"\e[0m\e[35m%s\e[0m\e[34m\" to package list...\e[0m\n" "$1"
echo "$1" >> "$NIXOS_CONFIG/home-ramak/packages.txt"
printf "\e[34m> Rebuild your system for the package to be installed.\e[0m\n"
