{ config, ... }:

{
  # Hyprland configuration
  xdg.configFile."xmonad/xmonad.hs" = config.util.dotFileMut "xmonad.hs";
}
