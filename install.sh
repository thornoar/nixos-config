#!/bin/sh
sudo chown -R $USER:root /etc/nixos/
mv /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
cp ./dotfiles/template.nix /etc/nixos/system-options.nix
cp ./dotfiles/template.nix /etc/nixos/home-options.nix
echo "NixOS setup is ready for use!"
echo "Call \"sudo nixos-rebuild switch --impure --flake $PWD#master\" to update system."
echo "Call \"home-manager switch --flake $PWD\" to update the user space."
