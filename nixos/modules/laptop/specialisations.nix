{ pkgs, pkgs-unstable, lib, readPackages, config, ... }@args: {
  specialisation.hyprland.configuration = import ./hyprland.nix args;
  specialisation.hyprland-powersave.configuration = import ./hyprland-powersave.nix args;
}
