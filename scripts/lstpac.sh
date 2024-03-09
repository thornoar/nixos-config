#!/bin/sh
echo -e "\e[35m| List Installed User Packages |\e[0m"
export GREP_COLORS="ms=1;94"
for package in $(<"$NIXOS_CONFIG/home-packages"); do
    nix search nixpkgs#"$package" | grep --color "$package"
done
export GREP_COLORS=""
