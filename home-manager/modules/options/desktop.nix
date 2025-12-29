{ config, lib, pkgs, modulesPath, ... }:

{
  home.pointerCursor.size = 16;

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
