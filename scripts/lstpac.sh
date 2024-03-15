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

let "width = $(stty -a | grep -Po '(?<=columns )\d+') - 19"

function ProgressBar {
    let "progress = ($1*$width)/$2"
    let "percentage = ($1*100)/$2"
    let "left = $width - $progress"

    _fill=$(printf "%${progress}s")
    _empty=$(printf "%${left}s")

    printf "\rProgress: [${_fill// /#}${_empty// /-}] [${percentage}%%]"
}

cur=0
end=$(wc -l < "$NIXOS_CONFIG/home-packages")

grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" > /dev/null 2>&1
    ((cur++))
    ProgressBar ${cur} ${end}
done

printf "\e[34m> Listing installed user packages...\e[0m\n"
grep -v '^ *#' < "$NIXOS_CONFIG/home-packages" | while IFS= read -r package
do
    nix search nixpkgs#"$package" | highlight 33 "$package"
done
