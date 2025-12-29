{ config, ... }:

let
  ts = builtins.toString;
  clr = config.colors;
in {
  # Hyprland configuration
  xdg.configFile."hypr/imports.conf".text = (config.util.toConf false "" config.window)
    + (config.util.toConf false "" config.misc) + (config.util.toConf false "" clr);
  xdg.configFile."wpaperd/config.toml".text = ''
    [${config.misc.monitorName}]
    path = "/home/ramak/media/wallpapers/${config.wallpaper.dir}"
    duration = "3m"
  '';
  xdg.configFile."hypr/hyprland.conf" = config.util.dotFileMut "hypr/hyprland.conf";

  # Waybar configuration
  xdg.configFile."waybar/shared.css".text = ''
    * {
        font-size: ${ts config.window.fontsizeWaybar}pt;
        border-radius: ${ts config.window.rounding}px;
        font-family: "${config.misc.systemFont}";
    }
  '';
  xdg.configFile."waybar/style.css" = config.util.dotFileMut "waybar/style.css";
  xdg.configFile."waybar/colors.css".text = (config.util.toCSS false "" config.colors);
  xdg.configFile."waybar/config".text = ''
    {
        "layer": "top",
        "margin-top": ${ts config.window.windowSpaceOuter},
        "margin-bottom": 0,
        "margin-left": ${ts config.window.windowSpaceOuter},
        "margin-right": ${ts config.window.windowSpaceOuter},
        "layer": "top",
        "spacing": 0,
        "height": 24,

        "include": [
            "~/.config/waybar/modules.json"
        ],

        "modules-left": [
            "hyprland/workspaces",
        ],

        "modules-center": [
            "pulseaudio",
            "clock",
            "backlight",
        ],

        "modules-right": [
            "hyprland/language",
            "network",
            "cpu",
            "memory",
            "battery", 
        ]
    }
  '';
  xdg.configFile."waybar/modules.json" = config.util.dotFileMut "waybar/modules.json";
  
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
