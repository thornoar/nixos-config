{ lib, pkgs, config, ... }:
{
    options = 
    let
        opt = lib.mkOption;
        tp = lib.types;
        mkOpt = typ: df: opt {
            type = tp."${typ}";
            default = df;
        };
        mkStrOpt = df: opt {
            type = tp.str;
            default = df;
        };
        ts = builtins.toString;
    in
    {
        fontsize = opt { type = tp.int; };
        fontsizeBar = opt { type = tp.int; };
        dotfiledir = mkOpt "path" ./dotfiles;
        wallpaperDir = mkStrOpt "Landscapes";
        wallpaperGamma = mkOpt "float" 0.9;
        wallpaperContrast = mkOpt "float" 1.0;
        windowSpace = mkOpt "int" 14;
        windowBorderWidth = mkOpt "int" 0;
        windowOpacity = mkOpt "float" 0.9;
        barheight = mkOpt "int" 35;
        magnifiedScale = mkOpt "float" 1.5;
        scratchpadWidth = mkStrOpt "2 % 3";
        scratchpadHeight = mkStrOpt "2 % 3";
        font = mkStrOpt "Hack";

        padding = opt {
            type = tp.attrs;
            default = { x = 4; y = 4; };
        };

        bgColor0 = mkStrOpt "#0b0c0d";
        bgColor1 = mkStrOpt "#2c3037";
        bgColor2 = mkStrOpt "#43565c";
        fgColor = mkStrOpt "#17a88b";
        brfgColor = mkStrOpt "#00bc96";

        colorBlack = mkStrOpt "#000000";

        colorWhite0 = mkStrOpt "#ffffff";
        colorWhite1 = mkStrOpt "#f8f8f2";
        colorWhite2 = mkStrOpt "#e6efff";
        colorWhite3 = mkStrOpt "#bbbbbb";
        colorWhite4 = mkStrOpt "#595f6b";

        colorYellow0 = mkStrOpt "#f1fa8c";
        colorYellow1 = mkStrOpt "#d19a66";
        colorYellow2 = mkStrOpt "#eebb9e";

        colorOrange0 = mkStrOpt "#ffb86c";
        colorOrange1 = mkStrOpt "#c28141";
        colorOrange2 = mkStrOpt "#d27141";

        colorRed0 = mkStrOpt "#ff5555";
        colorRed1 = mkStrOpt "#e86671";
        colorRed2 = mkStrOpt "#dd9999";

        colorBlue0 = mkStrOpt "#3070f0";
        colorBlue1 = mkStrOpt "#61afef";
        colorBlue2 = mkStrOpt "#6272a4";

        colorCyan = mkStrOpt "#56b6c2";

        colorGreen0 = mkStrOpt "#89a870";
        colorGreen1 = mkStrOpt "#329c48";
        colorGreen2 = mkStrOpt "#78971a";
        colorGreen4 = mkStrOpt "#98c379";

        colorMagenta0 = mkStrOpt "#ff79c6";
        colorMagenta1 = mkStrOpt "#bd93f9";
        colorMagenta2 = mkStrOpt "#c678dd";
        colorMagenta3 = mkStrOpt "#b16286";

        xmobarTemplate = mkStrOpt " %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% ";
        xmobarExtraCommands = mkStrOpt ''
        '';
        xmobarExtraOptions = mkStrOpt ''
        '';
        # alpha = ${ts (builtins.floor (255*config.windowOpacity))},
        xmobarOptions = mkStrOpt ''
            Config {
                font     = "xft:${config.font} Nerd Font Mono-${ts config.fontsizeBar}",
                bgColor  = "${config.bgColor0}",
                fgColor  = "${config.fgColor}",
                position = TopH ${ts config.barheight},
                persistent = False,
                hideOnStart = False,
                allDesktops = True,
                lowerOnStart = True,
                ${config.xmobarExtraOptions}
                commands = [
                    Run Alsa "default" "Master" [
                        "--template", "<fc=${config.colorWhite1}><volumestatus></fc>",
                        "--suffix", "True",
                        "--",
                        "--on", ""
                    ],
                    Run Date "<fc=${config.colorMagenta0}>%H:%M:%S</fc> | <fc=${config.colorMagenta1}>%a %Y-%m-%d</fc>" "date" 10,
                    Run XMonadLog,
                    ${config.xmobarExtraCommands}
                    Run Kbd [
                        ("us", "<fc=${config.colorWhite1}>US</fc>"),
                        ("ru", "<fc=${config.colorWhite1}>RU</fc>"),
                        ("de", "<fc=${config.colorWhite1}>DE</fc>")
                    ]
                ],
                sepChar  = "%",
                alignSep = "}{",
                template = "${config.xmobarTemplate}",
            }
        '';
    };
}
