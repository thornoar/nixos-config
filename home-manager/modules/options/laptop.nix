{ lib, ... }:

{
  hyprland = lib.mkForce {
    desktopScale = 2.0;
    widthPixels = 2880;
    heightPixels = 1620;
    resolution = "2880x1620";
    fontsize = 11;
    fontsizeWaybar = 11;
    windowSpaceInner = 0;
    windowSpaceOuter = 0;
    windowBorderWidth = 0;
    terminalOpacity = 0.92;
    terminalPaddingX = 1;
    terminalPaddingY = 1;
    terminalPadding = 1;
    rounding = 0;
    barHeight = 50;
    firefoxScale = 1.8;
  };

  wallpaper = lib.mkForce {
    dir = "Gruvbox";
    gamma = 0.9;
    contrast = 1.0;
  };

  misc = lib.mkForce {
    usePackageList = true;
    systemFont = "Hack Nerd Font Mono";
    monitorName = "eDP-1";
    wmStartupCommand =
      "sleep 8; echo disable > /sys/firmware/acpi/interrupts/gpe6F; sleep 1; echo disable > /sys/firmware/acpi/interrupts/gpe6F; sleep 1; echo disable > /sys/firmware/acpi/interrupts/gpe6F";
  };

  # xmonad = lib.mkForce {
  #   desktopScale = 1.0;
  #   fontsize = 22;
  #   fontsizeXmobar = 22;
  #   fontsizeRunPrompt = 11;
  #   windowSpaceInner = 4;
  #   windowSpaceOuter = 8;
  #   windowBorderWidth = 0;
  #   terminalOpacity = 0.9;
  #   terminalPaddingX = 3;
  #   terminalPaddingY = 3;
  #   terminalPadding = 3;
  #   barHeight = 50;
  #   magnifiedScale = 1.5;
  #   scratchpadWidth = "4 % 5";
  #   scratchpadHeight = "35 % 50";
  #   firefoxScale = 0.9;
  # };

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
  #         "-O", "<fc=${config.colors.colorYellow1}>Charging</fc>",
  #         "-i", "<fc=${config.colors.colorGreen0}>Charged</fc>"
  #     ] 50,
  #     Run Brightness [
  #         "-t", "<fc=${config.colors.colorMagenta1}>BRI: <percent>%</fc>", "--", "-D", "intel_backlight"
  #     ] 60,
  #   '';
  #   extraOptions = ""; # {
  #   template =
  #     " %XMonadLog% } %alsa:default:Master% | %date% { %bright% | %kbd% | %multicpu% | %memory% | %battery% "; # }
  # };
}
