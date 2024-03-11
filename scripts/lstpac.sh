#!/usr/bin/env bash

highlight() {
    escape=$(printf '\033')
    sed "s,$2,${escape}[$1m&${escape}[0m,g"
}

printf "\e[35m| List Installed User Packages |\e[0m\n"
grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" | highlight 33 "$package"
done
