#!/usr/bin/env bash

# freeopenvpn passwords: https://www.freeopenvpn.org/premium.php?cntid=Germany
# vpnbook passwords: https://www.vpnbook.com/freevpn

POSITIONAL_ARGS=()
port="2"
raw=0

function printUsage {
    printf "usage: vpn [ connect COUNTRY | disconnect | list | status | ip | set-password ]\n"
    printf "           [ -p|--port PORT | -h|--help | -r|--raw ]\n"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -p|--port) port="$2"; shift; shift ;;
        -r|--raw) raw=1; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

function interrupt_handler () {
    if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Interrupted by user.\n"; fi
    exit 1
}
trap interrupt_handler SIGINT

cmd="$1"

function getCountryName () {
    case "$1" in # ((((
        "us") printf "USA" ;;
        "nl") printf "Netherlands" ;;
        "jp") printf "Japan" ;;
        "de") printf "Germany" ;;
        "ca") printf "Canada" ;;
        *) printf "%s" "$1" ;;
    esac
}

function serverExists () {
    systemctl list-unit-files "openvpn-server-$1.service" --quiet >/dev/null
}

function serverActive () {
    systemctl is-active "$1" --quiet >/dev/null
}

function listServersUgly {
    systemctl list-unit-files --quiet | grep "openvpn-server" | awk '{print $1}'
}

function getCountryPort () {
    [[ $1 =~ openvpn-server-([a-z]*)-([0-9]*).service ]]
}

function printServer () {
    if getCountryPort "$1"; then
        printf "\e[1;34m#\e[0m \e[33m%s\e[0m on port \e[33m%s\e[0m.\e[35m%s\e[0m\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}" "$2"
    else
        printf "\e[1;31m#\e[0m Server exists but in wrong format: \e[35m%s\e[0m.\e[35m%s\e[0m\n" "$1" "$2"
    fi
}

function listServersPretty {
    for service in $(listServersUgly); do
        if serverActive "$service"; then
            printServer "$service" " (connected)"
        else
            printServer "$service" ""
        fi
    done
}

function disconnectAll () {
    for service in $(listServersUgly); do
        if systemctl is-active "$service" --quiet; then
            sudo systemctl stop "$service" || exit 1
            if getCountryPort "$service"; then
                printf "\e[1;$1m#\e[0m Disconnected from \e[33m%s\e[0m on port \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
            else
                printf "\e[1;$1m#\e[0m Disconnected from \e[35m%s\e[0m.\n" "$service"
            fi
        fi
    done
}

if [ -z "$cmd" ]; then
    if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No command given.\n"; fi
    exit 1
fi

if [[ "connect" =~ ^"$cmd" ]]; then
    country="$2"
    if ! serverExists "$country-$port"; then
        if [ "$raw" -eq 0 ]; then
            printf "\e[1;31m#\e[0m Country \e[33m%s\e[0m not supported on port \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$port"
        fi
        exit 1
    fi
    if serverActive "openvpn-server-$country-$port.service"; then
        if [ "$raw" -eq 0 ]; then
            printf "\e[1;31m#\e[0m Already connected to \e[33m%s\e[0m on port \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$port"
        else
            printf "%s-%s\n" "$country" "$port"
        fi
        exit 0
    fi
    disconnectAll "31"
    sudo systemctl start "openvpn-server-$country-$port.service" || exit 1
    if [ "$raw" -eq 0 ]; then
        printf "\e[1;34m#\e[0m Connected to \e[33m%s\e[0m on port \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$port"
    else
        printf "%s-%s\n" "$country" "$port"
    fi
elif [[ "disconnect" =~ ^"$cmd" ]]; then
    disconnectAll "34"
elif [[ "list" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then listServersPretty; else listServersUgly; fi
elif [[ "status" =~ ^"$cmd" ]]; then
    connected=0
    for service in $(listServersUgly); do
        if systemctl is-active "$service" --quiet; then
            if getCountryPort "$service"; then
                if [ "$raw" -eq 0 ]; then
                    printf "\e[1;34m#\e[0m Connection active in \e[33m%s\e[0m on port \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
                else
                    printf "%s-%s\n" "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
                fi
            else
                if [ "$raw" -eq 0 ]; then
                    printf "\e[1;34m#\e[0m Connection active in \e[35m%s\e[0m.\n" "$service"
                else
                    printf "%s\n" "$service"
                fi
            fi
            connected=1
            break
        fi
    done

    if [ "$connected" -eq 0 ]; then
        if [ "$raw" -eq 0 ]; then
            printf "\e[1;34m#\e[0m No connections active.\n"
        else
            printf "disconnected\n"
        fi
    fi
elif [[ "ip" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then getip public; else getip public --raw; fi
elif [[ "set-password" =~ ^"$cmd" ]]; then
    sudo -s "$(which vpn-change-passwords)"
else
    if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m Unknown command: \e[33m%s\e[0m.\n" "$cmd"; fi
    exit 1
fi
