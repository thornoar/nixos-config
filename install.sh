#!/usr/bin/env bash
# printf "\e[35| NixOS Configuration Install Script |\e[0m\n"

if [ -d "/home/$USER/projects/nixos-config" ]; then
    printf "\e[33m> NixOS configuration folder already detected at \e[35m~/projects/nixos-config/\e[33m. Remove and reinstall? (y/n)\e[0m "
    read ans
    if [ "$ans" != "${ans#[Yy]}" ]; then
        printf "\e[34m> Your NixOS configuration will be moved to \"\e[35m~/projects/old-nixos-config/\e[34m\".\e[0m\n"
        sudo mv "~/projects/nixos-config" "~/projects/old-nixos-config"
    else
        printf "\e[31mEnjoy your old configuration. I couldn't care less.\e[0m\n"
        exit 1
    fi
fi

printf "\e[34m> Cloning the git repository...\e[0m\n"
git clone https://github.com/thornoar/nixos-config ~/projects/nixos-config || exit 1

printf "\e[34m> Changing permissions for \"\e[35m/etc/nixos/\e[34m\"...\e[0m\n"
sudo chown -R "$USER":users /etc/nixos/ || exit 1

printf "\e[34m> Recreating hardware configuration...\e[0m\n"
sudo nixos-generate-config --show-hardware-config > /etc/nixos/hardware-configuration.nix || exit 1

printf "\e[34m> Setting up local configuration files in \"\e[35m/etc/nixos/\e[34m\"...\e[0m\n"
sudo mv /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak || exit 1
sudo cp ~/projects/nixos-config/dotfiles/template.nix /etc/nixos/system-local.nix || exit 1
sudo cp ~/projects/nixos-config/dotfiles/template.nix /etc/nixos/home-local.nix || exit 1

printf "\e[34m> Patching username...\e[0m\n"
sed "s/ramak/$USER" ~/projects/nixos-config/flake.nix || exit 1

printf "\e[34mNixOS setup is ready for use!\e[0m\n"
printf "\e[34mCall \"sudo nixos-rebuild switch --impure --flake ~/projects/nixos-config#master\" to update the system.\e[0m\n"
