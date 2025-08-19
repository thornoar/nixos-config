{ lib, config, ... }:

let
  dotfile = str: lib.path.append ../src str;
  ts = builtins.toString;
  toConf = mkstr:
    lib.attrsets.foldlAttrs (str: k: v:
      let
        v' =
          lib.strings.stringAsChars (x: if x == "#" then "0xff" else x) (ts v);
      in str + ''
        ''$${k} = ${if mkstr then ''"'' + v' + ''"'' else v'}
      '');
in {
  imports = [
    ./firefox.nix
    ./dispatch.nix
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
        "text/csv" = [ "sc-im.desktop" ];
        "text/html" = [ "firefox.desktop" ];
      };
      defaultApplications = associations.added;
    };

    # Hyprland configuration
    xdg.configFile."hypr/imports.conf".text = (toConf false "" config.hyprland)
      + (toConf false "" config.misc) + (toConf false "" config.colors);
    xdg.configFile."wpaperd/config.toml".text = ''
      [${config.misc.monitorName}]
      path = "/home/ramak/media/wallpapers/${config.wallpaper.dir}"
      duration = "3m"
    '';
    # xdg.configFile."hypr/hyprpaper.conf".source = dotfile "hypr/hyprpaper.conf";
    xdg.configFile."hypr/hyprland.conf".source = dotfile "hypr/hyprland.conf";

    # Waybar configuration
    xdg.configFile."waybar/size.css".text = ''
      * {
          font-size: ${ts config.hyprland.fontsizeWaybar}pt;
          border-radius: ${ts config.hyprland.rounding}px;
      }
    '';
    xdg.configFile."waybar/style.css".source = dotfile "waybar/style.css";
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
    xdg.configFile."waybar/modules.json".source = dotfile "waybar/modules.json";

    xdg.configFile."tofi/config".text = ''
      font-size = ${ts config.hyprland.fontsize}
      text-color = ${config.colors.colorWhite1}
      prompt-color =${config.colors.colorWhite1} 
      placeholder-color = ${config.colors.bgColor3}
      input-color =${config.colors.colorWhite1} 
      default-result-color =${config.colors.colorWhite1} 
      selection-color = ${config.colors.colorMagenta1}
      selection-background = ${config.colors.bgColor0}
      selection-match-color = ${config.colors.colorMagenta0}
      width = ${
        ts
        (config.hyprland.widthPixels / 2 - 2 * config.hyprland.windowSpaceOuter)
      }
      height = 50%
      background-color =${config.colors.bgColor0} 
      outline-width = 0
      outline-color =${config.colors.bgColor0} 
      border-width = 1
      border-color =${config.colors.colorMagenta1} 
      corner-radius = ${ts config.hyprland.rounding}
      padding-top = ${ts (config.hyprland.windowSpaceOuter / 2)}
      padding-bottom = ${ts (config.hyprland.windowSpaceOuter / 2)}
      padding-left = ${ts config.hyprland.windowSpaceOuter}
      padding-right = ${ts config.hyprland.windowSpaceOuter}
      margin-top = ${ts config.hyprland.windowSpaceOuter}
      margin-bottom = 0
      margin-left = ${ts config.hyprland.windowSpaceOuter}
      margin-right = ${ts config.hyprland.windowSpaceOuter}

    '' + builtins.readFile (dotfile "tofi.conf");

    programs.alacritty = {
      enable = true;
      settings = {
        colors.bright = {
          black = config.colors.bgColor3;
          blue = config.colors.colorBlue1;
          cyan = config.colors.colorCyan;
          green = config.colors.colorGreen1;
          magenta = config.colors.colorMagenta1;
          red = config.colors.colorRed1;
          white = config.colors.colorWhite1;
          yellow = config.colors.colorYellow1;
        };
        colors.dim = {
          black = config.colors.bgColor0;
          blue = config.colors.colorBlue1;
          cyan = config.colors.colorCyan;
          green = config.colors.colorGreen1;
          magenta = config.colors.colorMagenta1;
          red = config.colors.colorRed1;
          white = config.colors.bgColor3;
          yellow = config.colors.colorYellow1;
        };
        colors.normal = {
          black = config.colors.bgColor0;
          blue = config.colors.colorBlue1;
          cyan = config.colors.colorCyan;
          green = config.colors.colorGreen1;
          magenta = config.colors.colorMagenta1;
          red = config.colors.colorRed1;
          white = config.colors.colorWhite0;
          yellow = config.colors.colorYellow1;
        };
        colors.primary = {
          background = config.colors.bgColor0;
          bright_foreground = config.colors.brfgColor;
          foreground = config.colors.fgColor0;
        };
        cursor = { style = "Underline"; };
        env = {
          COLORTERM = "truecolor";
          TERM = "xterm-256color";
        };
        font = { size = config.hyprland.fontsize; };
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

