#!/usr/bin/env bash
nix search nixpkgs#"$1" > /dev/null 2>&1
nix search nixpkgs#"$1" | hl 33 "$1"
