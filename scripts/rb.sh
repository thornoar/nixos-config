#!/bin/sh
sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/$1
