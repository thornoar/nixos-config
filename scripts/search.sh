#!/usr/bin/env bash
nix search nixpkgs#"$1" | hl 33 "$1"
