#!/usr/bin/env bash

highlight() {
}

printf "\e[34m> Listing installed user packages...\e[0m\n"
grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" | highlight 33 "$package"
done
