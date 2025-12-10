{ config, lib, pkgs, modulesPath, ... }:

{
  home.pointerCursor.size = 16;

  hyprland = lib.mkForce {
    desktopScale = 1.6;
    resolution = "2560x1440";
    fontsize = 11;
    fontsizeWaybar = 11;
    windowSpaceInner = 4;
    windowSpaceOuter = 8;
    windowBorderWidth = 0;
    terminalOpacity = 0.9;
    terminalPadding = 1;
    rounding = 5;
    barHeight = 50;
    firefoxScale = 1.8;
  };

  wallpaper = lib.mkForce {
    dir = "Landscapes";
    gamma = 0.9;
    contrast = 1.0;
  };

  misc = lib.mkForce {
    usePackageList = true;
    systemFont = "Hack";
    monitorName = "HDMI-A-1";
    wmStartupCommand = "echo hi";
  };

  # xmobar = lib.mkForce {
  #   extraCommands = ''
  #     Run Battery [
  #         "--template" , "<acstatus>",
  #         "--Low"      , "10",        -- units: %
  #         "--High"     , "80",        -- units: %
  #         "--low"      , "red",
  #         "--normal"   , "orange",
  #         "--high"     , "green",
  #         "--",
  #         "-o", "<left>% (<timeleft>)",
  #         "-O", "<fc=${config.colors.yellow1}>Charging</fc>",
  #         "-i", "<fc=${config.colors.green0}>Charged</fc>"
  #     ] 50,
  #     Run Brightness [
  #         "-t", "<fc=${config.colors.magenta1}>BRI: <percent>%</fc>", "--", "-D", "intel_backlight"
  #     ] 60,
  #   '';
  #   extraOptions = ""; # {
  #   template =
  #     " %XMonadLog% } %alsa:default:Master% | %date% { %bright% | %kbd% | %multicpu% | %memory% | %battery% "; # }
  # };
}
