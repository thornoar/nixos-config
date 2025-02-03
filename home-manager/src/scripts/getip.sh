#!/usr/bin/env bash

function printUsage {
    printf "usage: getip [ public | private ] [ -r|--raw | -h|--help ]\n"
}

raw=0
while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -r|--raw) raw=1; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

cmd="$1"
if [ -z "$cmd" ]; then
    printUsage
    exit 0
fi

if [[ "private" =~ $cmd* ]]; then
    ip=$(ifconfig wlp46s0 | grep -i mask | awk '{print $2}'| cut -f2 -d:)
    if [ "$raw" == "0" ]; then
        printf "\e[1;34m#\e[0m Private IP: \e[33m%s\e[0m.\n" "$ip"
    else
        printf "%s\n" "$ip"
    fi
elif [[ "public" =~ $cmd* ]]; then
    json=$(curl ipinfo.io -s)
    ip=$(echo "$json" | jq --raw-output ".ip")
    if [ "$raw" == "0" ]; then
        country=$(echo "$json" | jq --raw-output ".country")
        city=$(echo "$json" | jq --raw-output ".city")
        org=$(echo "$json" | jq --raw-output ".org")
        loc=$(echo "$json" | jq --raw-output ".loc")
        printf "\e[1;34m#\e[0m Public IP: \e[33m%s\e[0m.\n" "$ip"
        printf "\e[1;34m#\e[0m Location: \e[33m%s\e[0m.\n" "$loc"
        printf "\e[1;34m#\e[0m Country: \e[33m%s\e[0m.\n" "$country"
        printf "\e[1;34m#\e[0m City: \e[33m%s\e[0m.\n" "$city"
        printf "\e[1;34m#\e[0m Organization: \e[33m%s\e[0m.\n" "$org"
    else
        printf "%s\n" "$ip"
    fi
elif [[ "all" =~ $cmd* ]]; then
    ip address show
else
    printf "\e[1;31m#\e[0m Unknown command: \e[33m%s\e[0m.\n" "$cmd"
    exit 1
fi
