#!/usr/bin/env bash

read -r -p "Enter VPN provider: " provider
if [ -z "$provider" ]; then
    printf "\e[1;31m#\e[0m Please provide a provider.\n"
    exit 1
fi
read -r -p "Enter new password: " new_password
if [ -z "$new_password" ]; then
    printf "\e[1;31m#\e[0m Please provide the new password.\n"
    exit 1
fi
for file in /root/nixos/openvpn/*.$provider.*.ovpn; do
    sed -i "N;s/^.*\\n\\(<\\/auth-user-pass>\\)/$new_password\\n\\1/;P;D" "$file"
done
