#!/usr/bin/env bash
if grep -q "^$1$" "$NIXOS_CONFIG/home-manager/src/packages/general.txt"; then
	printf "\e[1;34m#\e[0m Package \"\e[35m%s\e[0m\" already listed in configuration. Run \"\e[35msudo nixos-rebuild switch\e[0m\" to install. Quitting...\n" "$1" # ]]]]
	exit 0
fi
nix search nixpkgs#"$1" ^ || exit 1
printf "\e[1;34m# \e[0mAppending \"\e[0m\e[35m%s\e[0m\e[0m\" to package list...\n" "$1" # ]]]]]]
echo "$1" >> "$NIXOS_CONFIG/home-manager/src/packages/general.txt"
printf "\e[1;34m# \e[0mRebuild your system for the package to be installed.\n" # ]]
