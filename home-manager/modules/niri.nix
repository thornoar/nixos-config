{ config, lib, ... }:
let
  clr = config.colors;
  ts = builtins.toString;
in {
  xdg.configFile."niri/config.kdl" = config.util.dotFileMut "niri/config.kdl";
  xdg.configFile."niri/share.kdl".text = ''
    layout {
      gaps ${ts config.window.windowSpaceOuter}
      border {
        width 1
        active-color "${clr.primary}"
        inactive-color "${clr.bg0}"
        urgent-color "${clr.red1}"
      }
    }

    recent-windows {
      highlight {
        active-color "${clr.primary}"
        urgent-color "${clr.red1}"
        padding 30
        corner-radius 0
      }
    }
  '';
}
