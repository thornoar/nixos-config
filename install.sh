#!/usr/bin/env bash
# printf "\e[35| NixOS Configuration Install Script |\e[0m\n"

if [ -d "~/projects/nixos-config" ]; then
    printf "\e[33m> NixOS configuration folder already detected at \"\e[35m~/projects/nixos-config/\e[34m\". Do you want to delete it and reinstall? (y/n)\e[0m \n"
    select ync in "y" "n"; do
        case $ync in
            y )
                printf "\e[34m> Your NixOS configuration will be moved to \"\e[35m~/projects/old-nixos-config/\e[34m\".\e[0m\n"
                sudo mv "~/projects/nixos-config" "~/projects/old-nixos-config"
            ;;
            n )
                printf "\e[31mEnjoy your old configuration. I couldn't care less.\e[0m\n"
                exit 1
            ;;
        esac
    done
fi

printf "\e[34m> Cloning the git repository...\e[0m\n"
git clone https://github.com/thornoar/nixos-config ~/projects/nixos-config

printf "\e[34m> Changing permissions for \"\e[35m/etc/nixos/\e[34m\"...\e[0m\n"
sudo chown -R "$USER":users /etc/nixos/

printf "\e[34m> Recreating hardware configuration...\e[0m\n"
sudo nixos-generate-config --show-hardware-config > /etc/nixos/hardware-configuration.nix

printf "\e[34m> Setting up local configuration files in \"\e[35m/etc/nixos/\e[34m\"...\e[0m\n"
sudo mv /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
sudo cp ~/projects/nixos-config/dotfiles/template.nix /etc/nixos/system-local.nix
sudo cp ~/projects/nixos-config/dotfiles/template.nix /etc/nixos/home-local.nix

printf "\e[34m> Patching username...\e[0m\n"
sed "s/ramak/$(whoami)" ~/projects/nixos-config/flake.nix

printf "\e[34mNixOS setup is ready for use!\e[0m\n"
printf "\e[34mCall \"sudo nixos-rebuild switch --impure --flake ~/projects/nixos-config#master\" to update the system.\e[0m\n"
