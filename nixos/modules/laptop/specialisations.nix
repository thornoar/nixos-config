{ pkgs, lib, config, ... }@args: {
  specialisation.hyprland.configuration = import ./hyprland.nix args;
  specialisation.hyprland-powersave.configuration = import ./hyprland-powersave.nix args;
  specialisation.xmonad.configuration = import ./xmonad.nix args;
  environment.variables.SPECIALISATION_ENABLE = lib.mkForce "1";
}
