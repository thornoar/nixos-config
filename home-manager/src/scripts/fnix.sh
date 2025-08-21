#!/usr/bin/env bash

POSITIONAL_ARGS=()
raw=0
desc=0
level=""
section=""
given_level_section=0

function printUsage {
    printf "usage: fnix [ preview ..PACKAGES | install ..PACKAGES | remove ..PACKAGES | search ..PACKAGES ]\n"
    printf "            [ system-options | collect-garbage | list-installed ]\n"
    printf "            [ -h|--help | -r|--raw | -d|--desc ]\n"
    printf "            [ -l|--level user|system | -s|--section SECTION ]\n"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in #((((
        -r|--raw) raw=1; shift ;;
        -d|--desc) desc=1; shift ;;
        -l|--level) level="$2"; shift; shift ;;
        -s|--section) section="$2"; shift; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

if [ -n "$level" ] && [ -n "$section" ]; then
    given_level_section=1
fi

function interrupt_handler () {
    if [ "$raw" -eq 0 ]; then printf "\n\e[1;31m#\e[0m Interrupted by user.\n"; fi
    exit 1
}
trap interrupt_handler SIGINT

cmd="$1"

function hl () {
    escape=$(printf '\033')
    sed "s,$2,${escape}[$1m&${escape}[0m,g"
}

function getLevel () {
    case "$1" in # ((((
        "system") printf "nixos" ;;
        "user") printf "home-manager" ;;
        *) printf "%s" "$1" ;;
    esac
}

function printPackage () {
    pac="$1"
    json=$(nix search "nixpkgs#$pac" ^ --no-write-lock-file --quiet --offline --json 2>/dev/null)
    if [ -z "$json" ]; then
        if [ "$raw" -eq 0 ]; then
            printf "\e[1;31m#\e[0m Package not found: \e[33m%s\e[0m.\n" "$pac"
        fi
        return 1
    fi
    title=$(echo "$json" | jq -r '. | keys | .[0]')
    if [ "$raw" -eq 0 ]; then
        printf "\e[1;33m#\e[0m %s\e[0m\n" "$(echo "$title" | hl 33 "$pac")"
    else
        printf "%s\n" "$title"
    fi
    if [ "$desc" -eq 1 ]; then
        description=$(echo "$json" | jq ".\"$title\".description" --raw-output)
        printf "  %s\n" "$description"
    fi
}

function listPackages () {
    file="$1"
    base="$(basename "$file")"
    cur_section="${base%.*}"
    # cur_section="$2"
    if [ "$cur_section" == "custom" ]; then return 0; fi
    if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Listing packages in section \e[1;34m%s\e[0m:\n" "$cur_section"; fi
    while IFS= read -r pac; do
        if [ "$raw" -eq 0 ]; then
            printPackage "$pac"
        else
            echo "$pac"
        fi
    done < "$file"
    if [ "$raw" -eq 0 ]; then echo; fi
}

function queryForLevelAndSection () {
    if [ -z "$level" ]; then
        read -r -p '[1;35m#[0m Enter the package level: [35m' level
        printf "\e[0m"
    fi
    if [ -z "$section" ]; then
        read -r -p '[1;35m#[0m Section to put the package in: [35m' section
        printf "\e[0m"
    fi
}

function packageInstalled () {
    levelList=("system" "user")
    for cur_level in "${levelList[@]}"; do
        for file in "$NIXOS_CONFIG"/"$(getLevel "$cur_level")"/src/packages/*.txt; do
            if grep -q "^$1$" "$file"; then
                level="$cur_level"
                base="$(basename "$file")"
                section="${base%.*}"
                return 0
            fi
        done
    done
    return 1
}

function installPackage () {
    queryForLevelAndSection
    package="$1"
    file="$NIXOS_CONFIG/$(getLevel "$level")/src/packages/$section.txt"
    if [ ! -f "$file" ]; then
        if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Section \e[35m%s\e[0m not found on \e[35m%s\e[0m level.\n" "$section" "$level"; fi
        if [ "$given_level_section" -eq 1 ]; then
            installPackage "$package"
        else
            return 1
        fi
    fi
    echo "$package" >> "$file"
}

if [[ "system-options" =~ ^"$cmd" ]]; then
    manix "" | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf --preview="manix '{}'"
    exit 0
elif [[ "preview" =~ ^"$cmd" ]]; then
    packages=( "${@:2}" )
    if [ "${#packages[@]}" -eq 0 ]; then
        if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No packages given.\n"; fi
        exit 1
    fi
    for pac in "${packages[@]}"; do
        printPackage "$pac" || exit 1
    done
    packages=( "${packages[@]/#/nixpkgs#}" )
    if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Evaluating package derivations:\n"; fi
    nix shell --impure "${packages[@]}"
elif [[ "list-installed" =~ ^"$cmd" ]]; then
    levelList=("system" "user")
    if [ -n "$level" ]; then
        if echo "${levelList[@]}" | grep -q "$level"; then
            levelList=("$level")
        else
            if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Unknown level: \e[33m%s\e[0m.\n" "$level"; fi
            exit 1
        fi
    fi
    for cur_level in "${levelList[@]}"; do
        if [ "$raw" -eq 0 ]; then printf "\e[1;35m#\e[0m Listing packages on level \e[1;35m%s\e[0m:\n" "$cur_level"; fi
        if [ -n "$section" ]; then
            file="$NIXOS_CONFIG/$(getLevel "$cur_level")/src/packages/$section.txt"
            if [ -f "$file" ]; then
                listPackages "$file"
            else
                if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Section \e[35m%s\e[0m not found on \e[35m%s\e[0m level.\n" "$section" "$cur_level"; fi
                exit 1
            fi
        else
            for file in "$NIXOS_CONFIG"/"$(getLevel "$cur_level")"/src/packages/*.txt; do
                listPackages "$file"
            done
        fi
    done
elif [[ "install" =~ ^"$cmd" ]]; then
    packages=("${@:2}")
    if [[ ${#packages[@]} -eq 0 ]]; then
        if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No packages given.\n"; fi
        exit 1
    fi
    level_backup="$level"
    section_backup="$section"
    changed=0
    for package in "${packages[@]}"; do
        printPackage "$package" || continue
        if packageInstalled "$package"; then
            if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Package \e[33m%s\e[0m already listed on \e[35m%s\e[0m level under section \e[35m%s\e[0m.\n" "$package" "$level" "$section"; fi
            continue
            level="$level_backup"
            section="$section_backup"
        fi
        if installPackage "$package"; then
            changed=1
        fi
    done
    if [ "$raw" -eq 0 ] && [ "$changed" -eq 1 ]; then printf "\e[1;34m#\e[0m Rebuild the system to apply changes.\n"; fi
elif [[ "remove" =~ ^"$cmd" ]]; then
    packages=("${@:2}")
    if [[ ${#packages[@]} -eq 0 ]]; then
        if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No packages given.\n"; fi
        exit 1
    fi
    changed=0
    for package in "${packages[@]}"; do
        if ! packageInstalled "$package"; then
            if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Package \e[33m%s\e[0m not listed in configuration.\n" "$package"; fi
            continue
        fi
        changed=1
        if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Removing package \e[33m%s\e[0m listed on \e[35m%s\e[0m level under section \e[35m%s\e[0m.\n" "$package" "$level" "$section"; fi
        file="$NIXOS_CONFIG/$(getLevel "$level")/src/packages/$section.txt"
        awk "!/$package/" "$file" > temp && mv temp "$file"
    done
    if [ "$raw" -eq 0 ] && [ "$changed" -eq 1 ]; then printf "\e[1;34m#\e[0m Rebuild the system to apply changes.\n"; fi
elif [[ "collect-garbage" =~ ^"$cmd" ]]; then
    sudo printf ""
    if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Collecting garbage on the user level.\n"; fi # ]]
    nix-collect-garbage --delete-old
    if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Collecting garbage on the root level.\n"; fi # ]]
    sudo nix-collect-garbage --delete-old
    if [ "$raw" -eq 0 ]; then printf "\e[1;34m#\e[0m Deleting boot entries.\n"; fi # ]]
    nix-collect-garbage -d
elif [[ "search" =~ ^"$cmd" ]]; then
    packages=("${@:2}")
    if [ "$raw" -eq 0 ]; then desc=1; fi
    if [[ ${#packages[@]} -eq 0 ]]; then
        if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No packages given.\n"; fi
        exit 1
    fi
    for package in "${packages[@]}"; do
        printPackage "$package" || continue
    done
else
    if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Unknown command: \e[33m%s\e[0m.\n" "$cmd"; fi
fi
