#!/usr/bin/env bash
nix run nixpkgs#"$1" -- "${@:2}"
