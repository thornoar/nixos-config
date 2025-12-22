{ config, ... }:

let
  ts = builtins.toString;
in {
  imports = [
    ./firefox.nix
  ];

  config = {
    # xdg-mime configuration
    xdg.configFile."mimeapps.list".force = true;
    xdg.mimeApps = rec {
      enable = true;
      associations.added = {
        "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
        "audio/mpeg" = [ "mpv.desktop" ];
        "audio/mp3" = [ "mpv.desktop" ];
        "video/vnd.avi" = [ "mpv.desktop" ];
        "image/vnd.djvu+multipage" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
        "image/svg+xml" = [ "firefox.desktop" ];
        "image/jpeg" = [ "swayimg.desktop" ];
        "image/png" = [ "swayimg.desktop" ];
        "text/csv" = [ "sc-im.desktop" ];
        "text/html" = [ "firefox.desktop" ];
      };
      defaultApplications = associations.added;
    };

    # Hyprland configuration
    xdg.configFile."hypr/imports.conf".text = (config.util.toConf false "" config.hyprland)
      + (config.util.toConf false "" config.misc) + (config.util.toConf false "" config.colors);
    xdg.configFile."wpaperd/config.toml".text = ''
      [${config.misc.monitorName}]
      path = "/home/ramak/media/wallpapers/${config.wallpaper.dir}"
      duration = "3m"
    '';
    xdg.configFile."hypr/hyprland.conf" = config.util.dotFileMut "hypr/hyprland.conf";

    # Swayimg configuration
    xdg.configFile."swayimg/config" = config.util.dotFileMut "swayimg/config";
    xdg.configFile."swayimg/colors".text = ''
      [viewer]
      window = ${config.colors.bg0}ff
    '';

    # Waybar configuration
    xdg.configFile."waybar/size.css".text = ''
      * {
          font-size: ${ts config.hyprland.fontsizeWaybar}pt;
          border-radius: ${ts config.hyprland.rounding}px;
      }
    '';
    xdg.configFile."waybar/style.css" = config.util.dotFileMut "waybar/style.css";
    xdg.configFile."waybar/config".text = ''
      {
          "layer": "top",
          "margin-top": ${ts config.hyprland.windowSpaceOuter},
          "margin-bottom": 0,
          "margin-left": ${ts config.hyprland.windowSpaceOuter},
          "margin-right": ${ts config.hyprland.windowSpaceOuter},
          "layer": "top",
          "spacing": 0,

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

    # Tofi configuration
    xdg.configFile."tofi/config".text = ''
      font-size = ${ts config.hyprland.fontsize}
      text-color = ${config.colors.primary}
      prompt-color =${config.colors.primary} 
      placeholder-color = ${config.colors.bg3}
      input-color =${config.colors.primary} 
      default-result-color =${config.colors.primary} 
      selection-color = ${config.colors.fg0}
      selection-background = ${config.colors.bg0}
      selection-match-color = ${config.colors.magenta0}
      width = ${
        ts
        (config.hyprland.widthPixels / 2 - 2 * config.hyprland.windowSpaceOuter)
      }
      height = 50%
      background-color =${config.colors.bg0} 
      outline-width = 0
      outline-color =${config.colors.bg0} 
      border-width = 1
      border-color =${config.colors.primary} 
      corner-radius = ${ts config.hyprland.rounding}
      # padding-top = ${ts (config.hyprland.windowSpaceOuter / 2)}
      # padding-bottom = ${ts (config.hyprland.windowSpaceOuter / 2)}
      # padding-left = ${ts config.hyprland.windowSpaceOuter}
      # padding-right = ${ts config.hyprland.windowSpaceOuter}
      padding-top = 5
      padding-bottom = 0
      padding-left = 10
      padding-right = 10
      margin-top = ${ts config.hyprland.windowSpaceOuter}
      margin-bottom = 0
      margin-left = ${ts config.hyprland.windowSpaceOuter}
      margin-right = ${ts config.hyprland.windowSpaceOuter}

    '' + builtins.readFile ../src/tofi.conf;

    # Foot configuration
    xdg.configFile."foot/foot.ini" = config.util.dotFileMut "foot/foot.ini";
    xdg.configFile."foot/colors.ini".text = ''
    '';
    
    # Alacritty configuration
    programs.alacritty = {
      enable = true;
      settings = {
        colors = rec {
          normal = {
            black = config.colors.bg0;
            blue = config.colors.blue1;
            cyan = config.colors.cyan;
            green = config.colors.green1;
            magenta = config.colors.magenta1;
            red = config.colors.red1;
            white = config.colors.white0;
            yellow = config.colors.yellow1;
          };
          bright = normal;
          dim = normal;
          primary = {
            background = config.colors.bg0;
            bright_foreground = config.colors.fg1;
            foreground = config.colors.primary;
          };
        };
        cursor = { style = "Underline"; };
        env = {
          COLORTERM = "truecolor";
          TERM = "xterm-256color";
        };
        font = {
          size = config.hyprland.fontsize;
        };
        font.bold = {
          family = config.misc.systemFont;
          style = "Bold";
        };
        font.bold_italic = {
          family = config.misc.systemFont;
          style = "Bold Italic";
        };
        font.italic = {
          family = config.misc.systemFont;
          style = "Italic";
        };
        font.normal = {
          family = config.misc.systemFont;
          style = "Regular";
        };
        window.padding = {
          x = config.hyprland.terminalPaddingX;
          y = config.hyprland.terminalPaddingY;
        };
        window.dynamic_padding = true;
        window.opacity = config.hyprland.terminalOpacity;
        keyboard.bindings = [
          {
            key = "PageUp";
            action = "ScrollLineUp";
          }
          {
            key = "PageDown";
            action = "ScrollLineDown";
          }
          {
            key = "PageUp";
            mods = "Alt";
            action = "ScrollPageUp";
          }
          {
            key = "PageDown";
            mods = "Alt";
            action = "ScrollPageDown";
          }
        ];
      };
    };
  };
}

