#!/usr/bin/env bash

# freeopenvpn passwords: https://www.freeopenvpn.org/premium.php?cntid=Germany
# vpnbook passwords: https://www.vpnbook.com/freevpn

POSITIONAL_ARGS=()
provider=""
branch=""
raw=0

function printUsage {
    printf "usage: vpn [ connect COUNTRY | disconnect | list | status | ip | set-password ]\n"
    printf "           [ -b|--branch BRANCH | -p|--provider PROVIDER ]\n"
    printf "           [ -h|--help | -r|--raw ]\n"
}

if [ -z "$1" ]; then
    printUsage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -b|--branch) branch="$2"; shift; shift ;;
        -p|--provider) provider="$2"; shift; shift ;;
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

if [ -z "$cmd" ]; then
    if [ "$raw" -eq 0 ]; then printf "\e[1;31m#\e[0m No command given.\n"; fi
    exit 1
fi

function getCountryName () {
    case "$1" in # ((((((((((
        "us") printf "USA" ;;
        "nl") printf "Netherlands" ;;
        "jp") printf "Japan" ;;
        "de") printf "Germany" ;;
        "ca") printf "Canada" ;;
        "kr") printf "Korea" ;;
        "fr") printf "France" ;;
        "ru") printf "Russia" ;;
        "th") printf "Thailand" ;;
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

function getCountryBranchProvider () {
    [[ $1 =~ openvpn-server-([a-z]*)-([0-9]*)-([a-z]*).service ]]
}

function configureBranchAndProvider {
    country="$1"
    if [ -z "$provider" ] || [ -z "$branch" ]; then
        pattern=""
        if [ -z "$branch" ] && [ -z "$provider" ]; then
            pattern="openvpn-server-$country"
        elif [ -z "$provider" ]; then
            pattern="openvpn-server-$country-$branch"
        else
            pattern="openvpn-server-$country-\\([0-9]*\\)-$provider"
        fi

        while IFS='' read -r line; do
            if getCountryBranchProvider "$line"; then
                branch="${BASH_REMATCH[2]}"
                provider="${BASH_REMATCH[3]}"
                return 0
            else
                if [ "$raw" -eq 0 ]; then
                    printf "\e[1;31m#\e[0m Server exists but in wrong format: \e[35m%s\e[0m.\n" "$line"
                fi
                exit 1
            fi
        done < <(systemctl list-unit-files --quiet | grep "$pattern" | awk '{print $1}')
        if [ "$raw" -eq 0 ]; then
            if [ -z "$branch" ] && [ -z "$provider" ]; then
                printf "\e[1;31m#\e[0m Country \e[33m%s\e[0m not supported on any branch.\n" "$(getCountryName "$country")"
            elif [ -z "$provider" ]; then
                printf "\e[1;31m#\e[0m Country \e[33m%s\e[0m not supported on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$branch"
            else
                printf "\e[1;31m#\e[0m Country \e[33m%s\e[0m not supported by \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider"
            fi
        fi
        exit 1
    else
        if ! serverExists "$country-$branch-$provider"; then
            if [ "$raw" -eq 0 ]; then
                printf "\e[1;31m#\e[0m Country \e[33m%s\e[0m not supported by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
            fi
            exit 1
        fi
    fi
}

function printServer () {
    if getCountryBranchProvider "$1"; then
        printf "\e[1;34m#\e[0m \e[33m%s\e[0m on branch \e[33m%s\e[0m by \e[33m%s\e[0m.\e[35m%s\e[0m\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}" "$2"
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
            if getCountryBranchProvider "$service"; then
                printf "\e[1;$1m#\e[0m Disconnected from \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
            else
                printf "\e[1;$1m#\e[0m Disconnected from \e[35m%s\e[0m.\n" "$service"
            fi
        fi
    done
}

if [[ "connect" =~ ^"$cmd" ]]; then
    country="$2"
    configureBranchAndProvider "$country"
    if serverActive "openvpn-server-$country-$branch-$provider.service"; then
        if [ "$raw" -eq 0 ]; then
            printf "\e[1;31m#\e[0m Already connected to \e[33m%s\e[0m by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
        else
            printf "%s-%s-%s\n" "$country" "$branch" "$provider"
        fi
        exit 0
    fi
    disconnectAll "31"
    sudo systemctl start "openvpn-server-$country-$branch-$provider.service" || exit 1
    if [ "$raw" -eq 0 ]; then
        printf "\e[1;34m#\e[0m Connected to \e[33m%s\e[0m by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
    else
        printf "%s-%s-%s\n" "$country" "$branch" "$provider"
    fi
elif [[ "disconnect" =~ ^"$cmd" ]]; then
    disconnectAll "34"
elif [[ "list" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then listServersPretty; else listServersUgly; fi
elif [[ "status" =~ ^"$cmd" ]]; then
    connected=0
    for service in $(listServersUgly); do
        if systemctl is-active "$service" --quiet; then
            if getCountryBranchProvider "$service"; then
                if [ "$raw" -eq 0 ]; then
                    printf "\e[1;34m#\e[0m Connection active in \e[33m%s\e[0m on branch \e[33m%s\e[0m by \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
                else
                    printf "%s-%s-%s\n" "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
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
