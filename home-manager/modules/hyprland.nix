{ config, ... }:

let
  ts = builtins.toString;
  clr = config.colors;
in {
  # Hyprland configuration
  xdg.configFile."hypr/imports.conf".text = (config.util.toConf false "" config.window)
    + (config.util.toConf false "" config.misc) + (config.util.toConf false "" clr);
  xdg.configFile."hypr/hyprland.conf" = config.util.dotFileMut "hypr/hyprland.conf";
  
  # # Alacritty configuration
  # programs.alacritty = {
  #   enable = true;
  #   settings = {
  #     colors = rec {
  #       normal = {
  #         black = clr.bg0;
  #         blue = clr.blue1;
  #         cyan = clr.cyan;
  #         green = clr.green1;
  #         magenta = clr.magenta1;
  #         red = clr.red1;
  #         white = clr.white0;
  #         yellow = clr.yellow1;
  #       };
  #       bright = normal;
  #       dim = normal;
  #       primary = {
  #         background = clr.bg0;
  #         bright_foreground = clr.fg1;
  #         foreground = clr.primary;
  #       };
  #     };
  #     cursor = { style = "Underline"; };
  #     env = {
  #       COLORTERM = "truecolor";
  #       TERM = "xterm-256color";
  #     };
  #     font = {
  #       size = config.window.fontsize;
  #     };
  #     font.bold = {
  #       family = config.misc.systemFont;
  #       style = "Bold";
  #     };
  #     font.bold_italic = {
  #       family = config.misc.systemFont;
  #       style = "Bold Italic";
  #     };
  #     font.italic = {
  #       family = config.misc.systemFont;
  #       style = "Italic";
  #     };
  #     font.normal = {
  #       family = config.misc.systemFont;
  #       style = "Regular";
  #     };
  #     window.padding = {
  #       x = config.window.terminalPaddingX;
  #       y = config.window.terminalPaddingY;
  #     };
  #     window.dynamic_padding = true;
  #     window.opacity = config.window.terminalOpacity;
  #     keyboard.bindings = [
  #       {
  #         key = "PageUp";
  #         action = "ScrollLineUp";
  #       }
  #       {
  #         key = "PageDown";
  #         action = "ScrollLineDown";
  #       }
  #       {
  #         key = "PageUp";
  #         mods = "Alt";
  #         action = "ScrollPageUp";
  #       }
  #       {
  #         key = "PageDown";
  #         mods = "Alt";
  #         action = "ScrollPageDown";
  #       }
  #     ];
  #   };
  # };
}
