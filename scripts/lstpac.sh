#!/usr/bin/env bash
printf "\e[35m| List Installed User Packages |\e[0m\n"
export GREP_COLORS="ms=1;94"
grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" > /dev/null 2>&1
done
grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" | grep --color "$package"
done
export GREP_COLORS=""
