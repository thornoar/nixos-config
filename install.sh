#!/bin/sh
printf "\e[35| NixOS Configuration Install Script |\e[0m\n"
sudo chown -R "$USER":root /etc/nixos/
sudo mv /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
sudo cp ./dotfiles/template.nix /etc/nixos/system-options.nix
sudo cp ./dotfiles/template.nix /etc/nixos/home-options.nix
printf "\e[34mNixOS setup is ready for use!\e[0m\n"
printf "\e[34mCall \"sudo nixos-rebuild switch --impure --flake %s#master\" to update system.\e[0m\n" "$PWD"
printf "\e[34mCall \"home-manager switch --flake %s\" to update the user space.\e[0m\n" "$PWD"
