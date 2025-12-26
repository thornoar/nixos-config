#!/usr/bin/env bash

POSITIONAL_ARGS=()
provider=""
branch=""
country=""
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
        -c|--country) country="$2"; shift; shift ;;
        -r|--raw) raw=1; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

function interrupt_handler () {
    if [ "$raw" -eq 0 ]; then printf "\e[31mInterrupted by user.\e[0m\n"; fi
    exit 1
}
trap interrupt_handler SIGINT

cmd="$1"

if [ -z "$cmd" ]; then
    if [ "$raw" -eq 0 ]; then printf "! No command given.\n"; fi
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
    output=$(systemctl list-unit-files --quiet | grep "openvpn-server" $grepcountry $grepprovider $grepbranch | awk '{print $1}')
    grepcountry=""
    if [ -n "$country" ]; then
        output=$(echo "$output" | grep "$country")
    fi
    grepprovider=""
    if [ -n "$provider" ]; then
        output=$(echo "$output" | grep "$provider")
    fi
    grepbranch=""
    if [ -n "$branch" ]; then
        output=$(echo "$output" | grep "$branch")
    fi
    echo "$output"
}

function getCountryBranchProvider () {
    [[ $1 =~ openvpn-server-([a-z]*)-([0-9]*)-([a-z]*).service ]]
}

function configureBranchAndProvider {
    configurations=()
    if [ -z "$provider" ] || [ -z "$branch" ]; then
        pattern=""
        if [ -z "$branch" ] && [ -z "$provider" ]; then
            pattern="openvpn-server-$country"
        elif [ -z "$provider" ]; then
            pattern="openvpn-server-$country-$branch"
        else
            pattern="openvpn-server-$country-\\([0-9]*\\)-$provider"
        fi

        found="0"
        while IFS='' read -r line; do
            if getCountryBranchProvider "$line"; then
                branch="${BASH_REMATCH[2]}"
                provider="${BASH_REMATCH[3]}"
                found="1"
                configurations+=("$branch-$provider")
            else
                if [ "$raw" -eq 0 ]; then
                    printf "! Server exists but in wrong format: \e[35m%s\e[0m.\n" "$line"
                fi
                exit 1
            fi
        done < <(systemctl list-unit-files --quiet | grep "$pattern" | awk '{print $1}')
        if [ "$found" -eq 1 ]; then return 0; fi
        if [ "$raw" -eq 0 ]; then
            if [ -z "$branch" ] && [ -z "$provider" ]; then
                printf "! Country \e[33m\"%s\"\e[0m not supported on any branch.\n" "$(getCountryName "$country")"
            elif [ -z "$provider" ]; then
                printf "! Country \e[33m\"%s\"\e[0m not supported on branch \e[33m\"%s\"\e[0m.\n" "$(getCountryName "$country")" "$branch"
            else
                printf "! Country \e[33m\"%s\"\e[0m not supported by \e[33m\"%s\"\e[0m.\n" "$(getCountryName "$country")" "$provider"
            fi
        fi
        exit 1
    else
        if ! serverExists "$country-$branch-$provider"; then
            if [ "$raw" -eq 0 ]; then
                printf "! Country \e[33m\"%s\"\e[0m not supported by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
            fi
            exit 1
        fi
        configurations=("$branch-$provider")
    fi
}

function printServer () {
    if getCountryBranchProvider "$1"; then
        printf "> \e[33m%s\e[0m on branch \e[33m%s\e[0m by \e[33m%s\e[0m.\e[35m%s\e[0m\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}" "$2"
    else
        printf "! Server exists but in wrong format: \e[35m%s\e[0m.\e[35m%s\e[0m\n" "$1" "$2"
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
                printf "> Disconnected from \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
            else
                printf "> Disconnected from \e[35m%s\e[0m.\n" "$service"
            fi
        fi
    done
}

if [[ "connect" =~ ^"$cmd" ]]; then
    configureBranchAndProvider "$country"
    for config in "${configurations[@]}"; do
        if [[ $config =~ ([0-9]*)-([a-z]*) ]]; then
            branch="${BASH_REMATCH[1]}"
            provider="${BASH_REMATCH[2]}"
        else continue; fi
        if serverActive "openvpn-server-$country-$branch-$provider.service"; then
            if [ "$raw" -eq 0 ]; then
                printf "! Already connected to \e[33m%s\e[0m by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
            else
                printf "%s-%s-%s\n" "$country" "$branch" "$provider"
            fi
            exit 0
        fi
        disconnectAll
        if [ "$raw" -eq 0 ]; then
            printf "> Connecting to \e[33m%s\e[0m by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
        fi
        ip_before=$(curl ipinfo.io -s | jq ".ip" --raw-output)
        sudo systemctl start "openvpn-server-$country-$branch-$provider.service" || exit 1
        if [ "$raw" -eq 0 ]; then
            printf "> Checking if IP was re-routed:\n"
        fi
        for _ in $(seq 10); do
            ip_after=$(curl ipinfo.io -s | jq ".ip" --raw-output)
            if [ "$ip_before" == "$ip_after" ]; then
                if [ "$raw" -eq 0 ]; then printf "# Not yet...\n"; fi
            else
                if [ "$raw" -eq 0 ]; then
                    printf "> Connected. Checking internet. \n"
                fi
                unstable="0"
                for _ in $(seq 3); do
                    if [ "$(getip public -r)" == "$ip_after" ]; then
                        if [ "$raw" -eq 0 ]; then
                            printf "# Stable...\n"
                        fi
                        sleep 0.5
                    else
                        unstable="1"
                    fi
                done
                if [ "$unstable" -eq 1 ]; then break; else
                    if [ "$raw" -eq 0 ]; then
                        printf "> Internet connection stable.\n"
                    else
                        printf "%s-%s-%s\n" "$country" "$branch" "$provider"
                    fi
                fi
                notify-send "VPN connection to $(getCountryName "$country") established"
                exit 0
            fi
            sleep 1.0
        done
        sudo systemctl stop "openvpn-server-$country-$branch-$provider.service" || exit 1
        if [ "$raw" -eq 0 ]; then printf "! There was a problem connecting.\n"; fi
    done
    exit 1
elif [[ "disconnect" =~ ^"$cmd" ]]; then
    disconnectAll
elif [[ "list" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then listServersPretty; else listServersUgly; fi
elif [[ "status" =~ ^"$cmd" ]]; then
    connected=0
    for service in $(listServersUgly); do
        if systemctl is-active "$service" --quiet; then
            if getCountryBranchProvider "$service"; then
                if [ "$raw" -eq 0 ]; then
                    printf "> Connection active in \e[33m%s\e[0m on branch \e[33m%s\e[0m by \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
                else
                    printf "%s-%s-%s\n" "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
                fi
            else
                if [ "$raw" -eq 0 ]; then
                    printf "> Connection active in \e[35m%s\e[0m.\n" "$service"
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
            printf "> No connections active.\n"
        else
            printf "disconnected\n"
        fi
    fi
elif [[ "ip" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then getip public; else getip public --raw; fi
elif [[ "set-password" =~ ^"$cmd" ]]; then
    sudo -s "$(which vpn-change-passwords)"
else
    if [ "$raw" -eq 0 ]; then printf "! Unknown command: \e[33m%s\e[0m.\n" "$cmd"; fi
    exit 1
fi
