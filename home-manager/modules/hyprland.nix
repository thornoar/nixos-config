{ config, ... }:

let
  ts = builtins.toString;
  clr = config.colors;
in {
  # Hyprland configuration
  xdg.configFile."hypr/imports.conf".text = (config.util.toConf false "" config.window)
    + (config.util.toConf false "" config.misc) + (config.util.toConf false "" clr);
  xdg.configFile."hypr/hyprland.conf" = config.util.dotFileMut "hypr/hyprland.conf";
}
