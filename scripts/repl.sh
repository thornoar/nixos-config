#!/usr/bin/env bash
nix repl "$NIXOS_CONFIG#nixosConfigurations.master"
