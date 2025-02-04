#!/usr/bin/env bash

POSITIONAL_ARGS=()
raw=0

function printUsage {
    printf "usage: fnix [ system-options | preview ..PACKAGES ] [ -h|--help | -r|--raw ]\n"
}

function hl () {
    escape=$(printf '\033')
    sed "s,$2,${escape}[$1m&${escape}[0m,g"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -r|--raw) raw=1; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

cmd="$1"

if [[ "system-options" =~ $cmd* ]]; then
    manix "" | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf --preview="manix '{}'" | xargs manix
    exit 0
elif [[ "preview" =~ $cmd* ]]; then
    packages=${*:2}
    if [ "${#packages[@]}" -eq 0 ]; then
        if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No packages given.\n"; fi
        exit 1
    fi
    packages_patched=$(for arg in $packages; do printf " nixpkgs#%s" "$arg"; done)
    if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Evaluating package derivations.\n"; fi
    nix shell --impure$packages_patched
elif [[ "list-installed" =~ $cmd* ]]; then
    for pac in $(cat $NIXOS_CONFIG/home-manager/src/packages.txt); do
        if [ "$raw" -eq 0 ]; then
            nix search "nixpkgs#$pac" ^ | hl "33" "$pac"
        else
            echo "$pac"
        fi
    done
else
    if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Unknown command: \e[33m%s\e[0m\n" "$cmd"; fi
fi
