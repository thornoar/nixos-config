#!/usr/bin/env bash

# Links:
# https://publicvpnlist.com/

POSITIONAL_ARGS=()
provider=""
branch=""
country=""
raw=0
verify=1

function printUsage {
    printf "usage: vpn [ connect COUNTRY | disconnect | list | status | ip | clean | log ]\n"
    printf "           [ -b|--branch BRANCH | -p|--provider PROVIDER ]\n"
    printf "           [ -h|--help | -r|--raw | -s|--skip ]\n"
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
        -s|--skip) verify=0; shift ;;
        -h|--help) printUsage; exit 0 ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"
cmd="$1"

if [[ "log" =~ ^"$cmd" ]]; then
    if test -f /tmp/curvpnlog; then
        less /tmp/curvpnlog
    fi
    exit 0
fi

if [ "$USER" != "root" ]; then
    echo "This command must run in privileged mode."
    exit 1
fi

function interrupt_handler () {
    if [ "$raw" -eq 0 ]; then printf "\e[31mInterrupted by user.\e[0m\n"; fi
    disconnectAll
    cleanFiles
    exit 1
}
trap interrupt_handler SIGINT

if [ -z "$cmd" ]; then
    if [ "$raw" -eq 0 ]; then printf "! No command given.\n"; fi
    exit 1
fi

function getCountryName () {
    case "$1" in #(((((((((((
        "us") printf "USA" ;;
        "nl") printf "Netherlands" ;;
        "jp") printf "Japan" ;;
        "de") printf "Germany" ;;
        "ca") printf "Canada" ;;
        "kr") printf "Korea" ;;
        "fr") printf "France" ;;
        "ru") printf "Russia" ;;
        "th") printf "Thailand" ;;
        "sr") printf "Serbia" ;;
        "fn") printf "Finland" ;;
        *) printf "%s" "$1" ;;
    esac
}

function cleanFiles () {
    file=""
    if test -f /tmp/curvpnfile; then
        file="-$(cat /tmp/curvpnfile)"
    fi
    rm -f /tmp/curvpnfile /tmp/curvpnpid
    if test -f /tmp/curvpnlog; then
        mv /tmp/curvpnlog "/tmp/$(date +"%Y-%m-%d-%H-%M")$file-vpnlog"
    fi
}

function serverExists () {
    test -f "/root/nixos/openvpn-$1.ovpn"
}

function serverActive () {
    test -f /tmp/curvpnfile && [ "$(cat /tmp/curvpnfile)" == "$1" ]
}

function listServersUgly {
    find /root/nixos/openvpn -maxdepth 1 -type f -printf "%f\n"
}

function getCountryBranchProvider () {
    [[ $1 =~ ([a-z]*)-([0-9]*)-([a-z]*).ovpn ]]
}

function configureBranchAndProvider {
    if [ -z "$country" ]; then
        printf "! No country specified.\n"
        exit 1
    fi
    configurations=()
    if [ -z "$provider" ] || [ -z "$branch" ]; then
        pattern=""
        if [ -z "$branch" ] && [ -z "$provider" ]; then
            pattern="$country"
        elif [ -z "$provider" ]; then
            pattern="$country-$branch"
        else
            pattern="$country-\\([0-9]*\\)-$provider"
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
        done < <(find /root/nixos/openvpn -maxdepth 1 -type f -printf "%f\n" | grep "$pattern")
        if [ "$found" -eq 1 ]; then return 0; fi
        if [ "$raw" -eq 0 ]; then
            if [ -z "$branch" ] && [ -z "$provider" ]; then
                printf "! Country \e[33m%s\e[0m not supported on any branch.\n" "$(getCountryName "$country")"
            elif [ -z "$provider" ]; then
                printf "! Country \e[33m%s\e[0m not supported on branch \e[33m\"%s\"\e[0m.\n" "$(getCountryName "$country")" "$branch"
            else
                printf "! Country \e[33m%s\e[0m not supported by \e[33m\"%s\"\e[0m.\n" "$(getCountryName "$country")" "$provider"
            fi
        fi
        exit 1
    else
        if ! serverExists "$country-$branch-$provider"; then
            if [ "$raw" -eq 0 ]; then
                printf "! Country \e[33m%s\e[0m not supported by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
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
    for file in $(listServersUgly); do
        if serverActive "$file"; then
            printServer "$file" " (connected)"
        else
            printServer "$file" ""
        fi
    done
}

function disconnectAll () {
    if test -f /tmp/curvpnfile && test -f /tmp/curvpnpid; then
        file="$(cat /tmp/curvpnfile)"
        kill -SIGTERM "$(cat /tmp/curvpnpid)" &> /dev/null
        if getCountryBranchProvider "$file"; then
            printf "> Disconnected from \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}"
        else
            printf "> Disconnected from \e[35m%s\e[0m.\n" "$file"
        fi
    fi
}

if [[ "connect" =~ ^"$cmd" ]]; then
    country="$2"
    configureBranchAndProvider "$country"
    disconnectAll
    for config in "${configurations[@]}"; do
        if [[ $config =~ ([0-9]*)-([a-z]*) ]]; then
            branch="${BASH_REMATCH[1]}"
            provider="${BASH_REMATCH[2]}"
        else continue; fi

        if [ "$raw" -eq 0 ]; then
            printf "> Connecting to \e[33m%s\e[0m by \e[33m%s\e[0m on branch \e[33m%s\e[0m.\n" "$(getCountryName "$country")" "$provider" "$branch"
        fi

        ip_before=$(curl icanhazip.com -s)

        openvpn --config "/root/nixos/openvpn/$country-$branch-$provider.ovpn" > /tmp/curvpnlog &
        pid="$!"
        echo "$pid" > /tmp/curvpnpid
        echo "$country-$branch-$provider.ovpn" > /tmp/curvpnfile

        if [ "$verify" -eq 1 ]; then
            if [ "$raw" -eq 0 ]; then
                printf "> Checking if IP was re-routed:\n"
            fi
            for _ in $(seq 10); do
                ip_after=$(curl icanhazip.com -s)
                if [ "$ip_before" == "$ip_after" ]; then
                    if [ "$raw" -eq 0 ]; then printf "# Still same IP...\n"; fi
                elif [[ "$ip_after" =~ [0-9]+.[0-9]+.[0-9]+.[0-9]+ ]]; then
                    if [ "$raw" -eq 0 ]; then
                        printf "> New IP is %s. Checking internet. \n" "$ip_after"
                    fi
                    unstable="0"
                    for _ in $(seq 5); do
                        if [ "$(curl icanhazip.com -s)" == "$ip_after" ]; then
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
                    exit 0
                else
                    if [ "$raw" -eq 0 ]; then printf "# Lost connection...\n"; fi
                fi
                sleep 1.0
            done

            kill -SIGTERM "$pid" &> /dev/null
            cleanFiles

            if [ "$raw" -eq 0 ]; then printf "! There was a problem connecting.\n"; fi
        else
            exit 0
        fi
    done
    exit 1
elif [[ "disconnect" =~ ^"$cmd" ]]; then
    disconnectAll
    cleanFiles
elif [[ "clean" =~ ^"$cmd" ]]; then
    cleanFiles
elif [[ "list" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then listServersPretty; else listServersUgly; fi
elif [[ "status" =~ ^"$cmd" ]]; then
    if test -f /tmp/curvpnfile && test -f /tmp/curvpnpid; then
        file="$(cat /tmp/curvpnfile)"
        if [ "$raw" -eq 0 ]; then
            if getCountryBranchProvider "$file"; then
                printf "> Connection active in \e[33m%s\e[0m on branch \e[33m%s\e[0m by \e[33m%s\e[0m.\n" "$(getCountryName "${BASH_REMATCH[1]}")" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
            else
                printf "> Connection active with file \e[35m%s\e[0m.\n" "$file"
            fi
        else
            printf "%s\n" "$file"
        fi
    else
        if [ "$raw" -eq 0 ]; then
            printf "> No connections active.\n"
        fi
    fi
elif [[ "ip" =~ ^"$cmd" ]]; then
    if [ "$raw" -eq 0 ]; then getip public; else curl icanhazip.com; fi
# elif [[ "set-password" =~ ^"$cmd" ]]; then
#     sudo -s "$(which vpn-change-passwords)"
else
    if [ "$raw" -eq 0 ]; then printf "! Unknown command: \e[33m%s\e[0m.\n" "$cmd"; fi
    exit 1
fi
