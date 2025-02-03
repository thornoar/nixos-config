#!/usr/bin/env bash

POSITIONAL_ARGS=()
port="2"

cmd="$1"
country="$2"

function printUsage {
    printf "usage: vpn [ connect COUNTRY | disconnect | list ] [ -p|--port PORT ]\n"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -p|--port) port="$2"; shift; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

function getCountryName () {
    case "$1" in # ((((
        "us") printf "USA" ;;
        "nl") printf "Netherlands" ;;
        "jp") printf "Japan" ;;
        *) printf "%s" "$1" ;;
    esac
}

function serverExists () {
    systemctl list-unit-files "openvpn-server-$1.service" --quiet >/dev/null
}

function serverActive () {
    systemctl is-active "openvpn-server-$1.service" --quiet >/dev/null
}

function listServersUgly {
    systemctl list-unit-files --quiet | grep "openvpn-server" | awk '{print $1}'
}

function getCountryPort () {
    [[ $1 =~ openvpn-server-([a-z]*)-([0-9]*).service ]]
}

function listServersPretty {
    for service in $(listServersUgly); do
        if getCountryPort "$service"; then
            printf "\e[1;34m#\e[0m \e[33m%s\e[0m at port \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
        else
            printf "\e[1;31m#\e[0m Server exists but in wrong format: \e[35m%s\e[0m.\n" "$service"
        fi
    done
}

function disconnectAll () {
    for service in $(listServersUgly); do
        if systemctl is-active "$service" --quiet; then
            sudo systemctl stop "$service" || exit 1
            if getCountryPort "$service"; then
                printf "\e[1;$1m#\e[0m Disconnected from \e[33m%s\e[0m at port \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
            else
                printf "\e[1;$1m#\e[0m Disconnected from \e[35m%s\e[0m.\n" "$service"
            fi
        fi
    done
}

if [ -z "$cmd" ]; then
    printf "\e[1;31m#\e[0m No command given.\n"
    exit 1
fi

if [[ "connect" =~ $cmd* ]]; then
    if ! serverExists "$country-$port"; then
        printf "\e[1;31m#\e[0m Country \e[33m%s\e[0m not supported on port \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$port"
        exit 1
    fi
    if serverActive "$country-$port"; then
        printf "\e[1;31m#\e[0m Already connected to \e[33m%s\e[0m on port \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$port"
        exit 0
    fi
    disconnectAll "31"
    sudo systemctl start "openvpn-server-$country-$port.service" || exit 1
    printf "\e[1;34m#\e[0m Connected to \e[33m%s\e[0m on port \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$port"
elif [[ "disconnect" =~ $cmd* ]]; then
    disconnectAll "34"
elif [[ "list" =~ $cmd* ]]; then
    listServersPretty
else
    printf "\e[1;31m#\e[0m Unknown command: %s\n" "$cmd"
    exit 1
fi
