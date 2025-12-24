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
    dir = "Asymptotic";
    gamma = 0.9;
    contrast = 1.0;
  };

  misc = lib.mkForce {
    systemFont = "JetBrains Mono";
    monitorName = "eDP-1";
    # wmStartupCommand =
    #   "sleep 2; echo disable > /sys/firmware/acpi/interrupts/gpe6F; sleep 1; echo disable > /sys/firmware/acpi/interrupts/gpe6F; sleep 1; echo disable > /sys/firmware/acpi/interrupts/gpe6F";
  };
}
