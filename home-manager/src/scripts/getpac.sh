#!/usr/bin/env bash
if grep -q "^$1$" "$NIXOS_CONFIG/home-manager/src/packages.txt"; then
	printf "| \e[34mPackage \"\e[35m%s\e[34m\" already listed in configuration. Run \"sudo nixos-rebuild switch\" to install. Quitting...\e[0m\n" "$1" # ]]]]
	exit 0
fi
nix search nixpkgs#"$1" || exit 1
printf "| \e[34mAppending \"\e[0m\e[35m%s\e[0m\e[34m\" to package list...\e[0m\n" "$1" # ]]]]]]
echo "$1" >> "$NIXOS_CONFIG/home-manager/src/packages.txt"
printf "| \e[34mRebuild your system for the package to be installed.\e[0m\n" # ]]
