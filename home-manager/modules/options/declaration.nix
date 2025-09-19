{ pkgs, lib, config, ... }:

{
  options = let
    opt = lib.mkOption;
    ts = builtins.toString;
  in {
    colors = lib.mkOption {
      type = lib.types.attrs;
      default = rec {
        primary = "#d3d3d3";
        # primary = fgColor0;

        bgColor0 = "#0c0c10";
        bgColor1 = "#121215";
        bgColor2 = "#262728";
        bgColor3 = "#4d4e4f";
        commentColor = "#5c6370";
        fgColor0 = "#17a88b";
        # fgColor0 = primary;
        brfgColor = "#00bc96";
        # brfgColor = fgColor0;

        colorBlack = "#000000";

        colorWhite0 = "#ffffff";
        colorWhite1 = "#f8f8f2";
        colorWhite2 = "#e6efff";
        colorWhite3 = "#bbbbbb";
        colorWhite4 = "#595f6b";

        colorYellow0 = "#f1fa8c";
        colorYellow1 = "#d19a66";
        colorYellow2 = "#eebb9e";

        colorOrange0 = "#ffb86c";
        colorOrange1 = "#c28141";
        colorOrange2 = "#d27141";

        colorRed0 = "#ff5555";
        colorRed1 = "#e86671";
        colorRed2 = "#dd9999";

        colorBlue0 = "#3070f0";
        colorBlue1 = "#61afef";
        colorBlue2 = "#6272a4";

        colorCyan = "#56b6c2";

        colorGreen0 = "#89a870";
        colorGreen1 = "#329c48";
        colorGreen2 = "#78971a";
        colorGreen4 = "#98c379";

        colorMagenta0 = "#e772c1";
        colorMagenta1 = "#bd93f9";
        colorMagenta2 = "#c678dd";
        colorMagenta3 = "#b36596";
      };
    };

    hyprland = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    xmonad = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    wallpaper = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    xmobar = lib.mkOption {
      type = lib.types.attrs;
      default = {
        extraCommands = "";
        extraOptions = "";
        template = " %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% ";
      };
    };

    xmobarOptions = opt {
      type = lib.types.str;
      default = ''
        Config {
            font     = "xft:${config.misc.systemFont} Nerd Font Mono-${
              ts config.xmonad.fontsizeXmobar
            }",
            bgColor  = "${config.colors.bgColor0}",
            fgColor  = "${config.colors.fgColor0}",
            position = TopH ${ts config.xmonad.barHeight},
            persistent = False,
            hideOnStart = False,
            allDesktops = True,
            lowerOnStart = True,
            ${config.xmobar.extraOptions}
            commands = [
                Run Alsa "default" "Master" [
                    "--template", "<fc=${config.colors.primary}>VOL: <volumestatus></fc>",
                    "--suffix", "True",
                    "--",
                    "--on", ""
                ],
                Run Date "<fc=${config.colors.primary}>%H:%M:%S</fc> | <fc=${config.colors.colorMagenta0}>%a %Y-%m-%d</fc>" "date" 10,
                Run XMonadLog,
                ${config.xmobar.extraCommands}
                Run Kbd [
                    ("us", "<fc=${config.colors.primary}>US</fc>"),
                    ("ru", "<fc=${config.colors.primary}>RU</fc>"),
                    ("de", "<fc=${config.colors.primary}>DE</fc>")
                ],
                Run MultiCpu       [ "--template" , "<fc=${config.colors.primary}>CPU:</fc> <total0><fc=${config.colors.colorMagenta1}>%</fc>"
                     , "--Low"      , "50"         -- units: %
                     , "--High"     , "85"         -- units: %
                     , "--low"      , "green"
                     , "--normal"   , "orange"
                     , "--high"     , "red"
                 ] 10,
                Run Memory         [ "--template" ,"<fc=${config.colors.primary}>RAM:</fc> <usedratio><fc=${config.colors.colorMagenta1}>%</fc>"
                    , "--Low"      , "20"        -- units: %
                    , "--High"     , "90"        -- units: %
                    , "--low"      , "darkgreen"
                    , "--normal"   , "darkorange"
                    , "--high"     , "red"
                ] 10
            ],
            sepChar  = "%",
            alignSep = "}{",
            template = "${config.xmobar.template}",
        }
      '';
    };

    misc = lib.mkOption {
      type = lib.types.attrs;
      default = {
        usePackageList = true;
        systemFont = "Hack";
      };
    };

    util = lib.mkOption {
      type = lib.types.attrs;
      default = rec {
        srcPath = /home/ramak/projects/nixos-config/home-manager/src;
        dotFileImmut = pkgs.dotFile (x: x) ../src;
        dotFileMut = pkgs.dotFile config.lib.file.mkOutOfStoreSymlink srcPath;
      };
    };
  };
}
