{ config, lib, pkgs, modulesPath, ... }:

{
  home.pointerCursor.size = 16;

  # hyprland = lib.mkForce {
  #   desktopScale = 1.6;
  #   resolution = "2560x1440";
  #   fontsize = 11;
  #   fontsizeWaybar = 11;
  #   windowSpaceInner = 4;
  #   windowSpaceOuter = 8;
  #   windowBorderWidth = 0;
  #   terminalOpacity = 0.9;
  #   terminalPadding = 1;
  #   rounding = 5;
  #   barHeight = 50;
  #   firefoxScale = 1.8;
  # };

  wallpaper = lib.mkForce {
    dir = "Landscapes";
    gamma = 0.9;
    contrast = 1.0;
  };

  misc = lib.mkForce {
    systemFont = "Hack";
    monitorName = "HDMI-A-1";
    wmStartupCommand = "echo hi";
  };
}
