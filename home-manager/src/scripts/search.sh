#!/usr/bin/env bash

mode="exact"
while getopts "a" option; do
    case "$option" in
        a) mode="fuzzy" ;;
        *) printf "\e[1;31merror:\e[0m Invalid option: %s.\n" "$option"; exit 1 ;;
    esac
done

if [ "$mode" == "exact" ]; then
    nix search nixpkgs\#"$1" ^ | hl 33 "$1"
else
    nix search nixpkgs "$1" | hl 33 "$1"
fi
