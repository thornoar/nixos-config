# Roma's NixOS configuration

This repository contains most of how my system is built (excluding the configs containing secrets).

The system-level configuration lives in the `nixos` directory, with `configuration.nix` being the main module and other modules available in the `modules` subdirectory.

The user-level configuration lives in `home-manager` and works using [Home Manager](https://nix-community.github.io/home-manager/) to configure user files, programs, etc. The main module is `home.nix`, with additional modules in the `modules` subdirectory.

All of the above mentioned Nix modules are pieced together in `flake.nix` which defines
- The `inputs` --- references to Nix package repositories, and
- The `outputs` --- NixOS system derivations which include certain modules from the `nixos` and `home-manager` directories.

Both the `nixos` and `home-manager` directories have a `src` subdirectory which contains all the "source files" --- mostly configuration files for external programs such as Neovim or Htop. These files are then symlinked against by Nix, which allows to configure all of the user's programs in the same repository and then dynamically reconfigure without rebuilding the system.

The `secrets` directory contains enciphered secrets related to the configuration. The files are encrypted using the [pshash](https://github.com/thornoar/pshash) algorithm.
