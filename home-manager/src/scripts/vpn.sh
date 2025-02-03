#!/usr/bin/env bash

cmd="$1"
coutries=("us" "nl" "jp")
country="us"
server="2"

while getopts "s:c:" option; do
    case "$option" in # ((
        c) country="$OPTARG" ;;
        s) server="$OPTARG" ;;
        *) printf "\e[1;31m#\e[0m Invalid option: %s.\n" "$option"; exit 1 ;;
    esac
done

if [ -z "$cmd" ]; then
    printf "\e[1;31m#\e[0m No command given.\n"
    exit 1
fi

if [ "$cmd" == "connect" ]; then
    for countryp in "${coutries[@]}"; do
        if sudo systemctl is-active "openvpn-$countryp-free-$server.service" --quiet; then
            printf "| "
            sudo systemctl stop "openvpn-$countryp-free-$server.service"
        fi
    done
    sudo systemctl start "openvpn-$country-free-$server.service"
elif [ "$cmd" == "disconnect" ]; then
    for countryp in "${coutries[@]}"; do
        sudo systemctl is-active "openvpn-$countryp-free-$server.service" --quiet && sudo systemctl stop "openvpn-$countryp-free-$server.service"
    done
else
    printf "\e[1;31m#\e[0m Unknown command: %s\n" "$cmd"
    exit 1
fi
