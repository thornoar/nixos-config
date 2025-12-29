{ config, lib, ... }:

let
  ts = builtins.toString;
  bc = lib.strings.removePrefix "#";
  clr = config.colors;
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
    xdg.configFile."hypr/imports.conf".text = (config.util.toConf false "" config.window)
      + (config.util.toConf false "" config.misc) + (config.util.toConf false "" clr);
    xdg.configFile."wpaperd/config.toml".text = ''
      [${config.misc.monitorName}]
      path = "/home/ramak/media/wallpapers/${config.wallpaper.dir}"
      duration = "3m"
    '';
    xdg.configFile."hypr/hyprland.conf" = config.util.dotFileMut "hypr/hyprland.conf";

    # Swayimg configuration
    xdg.configFile."swayimg/config" = config.util.dotFileMut "swayimg/config";
    xdg.configFile."swayimg/shared".text = ''
      [viewer]
      window = ${clr.bg0}ff

      [font]
      name = ${config.misc.systemFont}
      size = ${ts config.window.fontsize}
      color = #00000000
      shadow = #00000000
      background = #00000000
    '';

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

    # Tofi configuration
    xdg.configFile."tofi/config".text = ''
      font = "${config.misc.systemFont}"
      font-size = ${ts config.window.fontsize}
      text-color = ${clr.primary}
      prompt-color =${clr.primary} 
      placeholder-color = ${clr.bg3}
      input-color =${clr.primary} 
      default-result-color =${clr.primary} 
      selection-color = ${clr.fg0}
      selection-background = ${clr.bg0}
      selection-match-color = ${clr.magenta0}
      width = ${
        ts
        (config.window.widthPixels / 2 - 2 * config.window.windowSpaceOuter)
      }
      height = 50%
      background-color =${clr.bg0} 
      outline-width = 0
      outline-color =${clr.bg0} 
      border-width = 1
      border-color =${clr.primary} 
      corner-radius = ${ts config.window.rounding}
      # padding-top = ${ts (config.window.windowSpaceOuter / 2)}
      # padding-bottom = ${ts (config.window.windowSpaceOuter / 2)}
      # padding-left = ${ts config.window.windowSpaceOuter}
      # padding-right = ${ts config.window.windowSpaceOuter}
      padding-top = 5
      padding-bottom = 0
      padding-left = 10
      padding-right = 10
      margin-top = ${ts config.window.windowSpaceOuter}
      margin-bottom = 0
      margin-left = ${ts config.window.windowSpaceOuter}
      margin-right = ${ts config.window.windowSpaceOuter}

    '' + builtins.readFile ../src/tofi.conf;

    # Foot configuration
    xdg.configFile."foot/foot.ini" = config.util.dotFileMut "foot/foot.ini";
    xdg.configFile."foot/share.ini".text = ''
      font=${config.misc.systemFont}:size=${ts config.window.fontsize}, Noto Color Emoji

      pad=0x0

      [colors]
      alpha=${ts config.window.terminalOpacity}
      alpha-mode=default # Can be `default`, `matching` or `all`
      background=${bc clr.bg0}
      foreground=${bc clr.primary}

      regular0=${bc clr.black}    # black
      regular1=${bc clr.red1}     # red
      regular2=${bc clr.green1}   # green
      regular3=${bc clr.yellow1}  # yellow
      regular4=${bc clr.blue1}    # blue
      regular5=${bc clr.magenta1} # magenta
      regular6=${bc clr.cyan}     # cyan
      regular7=${bc clr.white0}   # white

      bright0=${bc clr.black}       # bright black
      bright1=${bc clr.red1}        # bright red
      bright2=${bc clr.green1}      # bright green
      bright3=${bc clr.yellow1}     # bright yellow
      bright4=${bc clr.blue1}       # bright blue
      bright5=${bc clr.magenta1}    # bright magenta
      bright6=${bc clr.cyan}        # bright cyan
      bright7=${bc clr.white0}      # bright white

      # dim-blend-towards=black
      dim0=${bc clr.black}   
      dim1=${bc clr.red1}    
      dim2=${bc clr.green1}  
      dim3=${bc clr.yellow1} 
      dim4=${bc clr.blue1}   
      dim5=${bc clr.magenta1}
      dim6=${bc clr.cyan}    
      dim7=${bc clr.white0}  

      ## Sixel colors
      # sixel0 =  000000
      # sixel1 =  3333cc
      # sixel2 =  cc2121
      # sixel3 =  33cc33
      # sixel4 =  cc33cc
      # sixel5 =  33cccc
      # sixel6 =  cccc33
      # sixel7 =  878787
      # sixel8 =  424242
      # sixel9 =  545499
      # sixel10 = 994242
      # sixel11 = 549954
      # sixel12 = 995499
      # sixel13 = 549999
      # sixel14 = 999954
      # sixel15 = cccccc

      ## Misc colors
      # selection-foreground=<inverse foreground/background>
      # selection-background=<inverse foreground/background>
      # jump-labels=regular0 regular3               # black-on-yellow
      # scrollback-indicator=<regular0> <bright4>   # black-on-bright-blue
      # search-box-no-match=<regular0> <regular1>   # black-on-red
      # search-box-match=<regular0> <regular3>      # black-on-yellow
      # urls=<regular3>
    '';
    
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
  };
}
