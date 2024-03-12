#!/usr/bin/env bash

while getopts "hs" flag; do
    case $flag in
        h)
            echo "Usage: lstpac [OPTION]"
            echo "List installed user-level packages on a NixOS system"
            echo "Example: lstpac -s"
            echo ""
            echo "Available options:"
            echo "  -h              Display this [h]elp message"
            echo "  -s              [S]imply return the list of packages, not showing info"
            exit 0
        ;;
        s)
            cat "$NIXOS_CONFIG/home-packages"
            exit 0
        ;;
        *) exit 1
        ;;
    esac
done

highlight() {
    escape=$(printf '\033')
    sed "s,$2,${escape}[$1m&${escape}[0m,g"
}

grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" > /dev/null 2>&1
done

printf "\e[34m> Listing installed user packages...\e[0m\n"
grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" | highlight 33 "$package"
done
