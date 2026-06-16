#!/usr/bin/env bash

export PATH='/home/ramak/.local/bin:/run/wrappers/bin:/home/ramak/.nix-profile/bin:/nix/profile/bin:/home/ramak/.local/state/nix/profile/bin:/etc/profiles/per-user/ramak/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:/home/ramak/media/sandbox/bin:/home/ramak/.local/share/cargo/bin'
export XDG_RUNTIME_DIR=/run/user/1000

mpd --kill

transmission-remote --exit
# pkill transmission-daemon
