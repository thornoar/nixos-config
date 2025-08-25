#!/usr/bin/env bash

function printUsage {
    printf "usage: getip [ public | private ] [ -r|--raw | -h|--help ]\n"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

raw=0
while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -r|--raw) raw=1; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

function interrupt_handler () {
    if [ "$raw" -eq 0 ]; then printf "\e[31mInterrupted by user.\e[0m\n"; fi # ]]
    exit 1
}
trap interrupt_handler SIGINT

cmd="$1"

if [[ "private" =~ $cmd ]]; then
    ip=$(ifconfig wlp46s0 | grep -i mask | awk '{print $2}'| cut -f2 -d:)
    if [ "$raw" -eq 0 ]; then
        printf "# Private IP: \e[33m%s\e[0m.\n" "$ip"
    else
        printf "%s\n" "$ip"
    fi
elif [[ "public" =~ "$cmd" ]]; then
    json=$(curl ipinfo.io -s)
    ip=$(echo "$json" | jq --raw-output ".ip")
    if [ "$raw" -eq 0 ]; then
        country=$(echo "$json" | jq --raw-output ".country")
        city=$(echo "$json" | jq --raw-output ".city")
        org=$(echo "$json" | jq --raw-output ".org")
        loc=$(echo "$json" | jq --raw-output ".loc")
        printf "# Public IP: \e[33m%s\e[0m.\n" "$ip"
        printf "# Location: \e[33m%s\e[0m.\n" "$loc"
        printf "# Country: \e[33m%s\e[0m.\n" "$country"
        printf "# City: \e[33m%s\e[0m.\n" "$city"
        printf "# Organization: \e[33m%s\e[0m.\n" "$org"
    else
        printf "%s\n" "$ip"
    fi
elif [[ "all" =~ "$cmd" ]]; then
    ip address show
else
    printf "! Unknown command: \e[33m%s\e[0m.\n" "$cmd"
    exit 1
fi
