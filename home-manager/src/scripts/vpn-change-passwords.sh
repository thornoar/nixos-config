#!/usr/bin/env bash

read -r -p "Enter VPN provider: " provider
if [ -z "$provider" ]; then
    printf "Please provide a provider.\n"
    exit 1
fi
read -r -p "Enter the relevant country: " country
if [ -z "$country" ]; then
    country="*"
fi
read -r -p "Enter new password: " new_password
if [ -z "$new_password" ]; then
    printf "Please provide the new password.\n"
    exit 1
fi
for file in /root/nixos/openvpn/$country-*.$provider.*.ovpn; do
    if [ -f "$file" ]; then
        sed -i "N;s/^.*\\n\\(<\\/auth-user-pass>\\)/$new_password\\n\\1/;P;D" "$file"
    fi
done
