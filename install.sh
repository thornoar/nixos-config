#!/usr/bin/env bash
printf "\e[35| NixOS Configuration Install Script |\e[0m\n"

if [ -d "/home/$USER/projects/nixos-config" ]; then
    printf "\e[33m> NixOS configuration folder already detected at \e[35m~/projects/nixos-config/\e[33m. Remove and reinstall? (y/n)\e[0m "
    read -r ans
    if [ "$ans" != "${ans#[Yy]}" ]; then
        printf "\e[34m> Your NixOS configuration will be moved to \"\e[35m~/projects/old-nixos-config/\e[34m\".\e[0m\n"
        sudo mv "$HOME/projects/nixos-config" "$HOME/projects/old-nixos-config"
    else
        printf "\e[31mEnjoy your old configuration. I couldn't care less.\e[0m\n"
        exit 1
    fi
fi

printf "\e[34m> Cloning main config repository...\e[0m\n"
git clone https://github.com/thornoar/nixos-config ~/projects/nixos-config || exit 1

read -p -r "Specify the local configuration to install: " local

printf "\e[34m> Cloning local config repository for device \e[35m%s\e[34m...\e[0m" "$local"
git clone https://github.com/thornoar/nixos-$local-config ~/projects/nixos-local-config || exit 1

printf "\e[34mNixOS setup is ready for use!\e[0m\n"
printf "\e[34mCall \"sudo nixos-rebuild switch --impure --flake ~/projects/nixos-config#master\" to update the system.\e[0m\n"
