#!/bin/sh
sudo chown -R $USER:root /etc/nixos/
sudo mv /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
sudo cp ./dotfiles/template.nix /etc/nixos/system-options.nix
sudo cp ./dotfiles/template.nix /etc/nixos/home-options.nix
echo "NixOS setup is ready for use!"
echo "Call \"sudo nixos-rebuild switch --impure --flake $PWD#master\" to update system."
echo "Call \"home-manager switch --flake $PWD\" to update the user space."
