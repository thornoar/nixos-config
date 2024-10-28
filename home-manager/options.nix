{ lib, pkgs, config, ... }:

{
    options = 
    let
        opt = lib.mkOption;
        tp = lib.types;
        ts = builtins.toString;
    in
    {
        colors = lib.mkOption {
            type = lib.types.attrs;
            default = {
                bgColor0            = "#0b0c0d";
                bgColor1            = "#0b0c0d";
                # bgColor1            = "#141516";
                bgColor2            = "#262728";
                bgColor3            = "#4d4e4f";
                fgColor             = "#17a88b";
                brfgColor           = "#00bc96";

                colorBlack          = "#000000";

                colorWhite0         = "#ffffff";
                colorWhite1         = "#f8f8f2";
                colorWhite2         = "#e6efff";
                colorWhite3         = "#bbbbbb";
                colorWhite4         = "#595f6b";

                colorYellow0        = "#f1fa8c";
                colorYellow1        = "#d19a66";
                colorYellow2        = "#eebb9e";

                colorOrange0        = "#ffb86c";
                colorOrange1        = "#c28141";
                colorOrange2        = "#d27141";

                colorRed0           = "#ff5555";
                colorRed1           = "#e86671";
                colorRed2           = "#dd9999";

                colorBlue0          = "#3070f0";
                colorBlue1          = "#61afef";
                colorBlue2          = "#6272a4";

                colorCyan           = "#56b6c2";

                colorGreen0         = "#89a870";
                colorGreen1         = "#329c48";
                colorGreen2         = "#78971a";
                colorGreen4         = "#98c379";

                colorMagenta0       = "#ff79c6";
                colorMagenta1       = "#bd93f9";
                colorMagenta2       = "#c678dd";
                colorMagenta3       = "#b16286";
            };
        };
        
        size = lib.mkOption {
            type = lib.types.attrs;
            default = {};
        };
        
        wallpaper = lib.mkOption {
            type = lib.types.attrs;
            default = {};
        };
        
        xmobar = lib.mkOption {
            type = lib.types.attrs;
            default = {
                extraCommands = "";
                extraOptions  = "";
                template = " %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% ";
            };
        };

        xmobarOptions = opt {
            type = lib.types.str;
            default =  ''
                Config {
                    font     = "xft:${config.misc.font} Nerd Font Mono-${ts config.size.fontsizeBar}",
                    bgColor  = "${config.colors.bgColor0}",
                    fgColor  = "${config.colors.fgColor}",
                    position = TopH ${ts config.size.barHeight},
                    persistent = False,
                    hideOnStart = False,
                    allDesktops = True,
                    lowerOnStart = True,
                    ${config.xmobar.extraOptions}
                    commands = [
                        Run Alsa "default" "Master" [
                            "--template", "<fc=${config.colors.colorWhite1}><volumestatus></fc>",
                            "--suffix", "True",
                            "--",
                            "--on", ""
                        ],
                        Run Date "<fc=${config.colors.colorMagenta0}>%H:%M:%S</fc> | <fc=${config.colors.colorMagenta1}>%a %Y-%m-%d</fc>" "date" 10,
                        Run XMonadLog,
                        ${config.xmobar.extraCommands}
                        Run Kbd [
                            ("us", "<fc=${config.colors.colorWhite1}>US</fc>"),
                            ("ru", "<fc=${config.colors.colorWhite1}>RU</fc>"),
                            ("de", "<fc=${config.colors.colorWhite1}>DE</fc>")
                        ]
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
                # fontsize            = opt { type = tp.int;      };
                # fontsizeBar         = opt { type = tp.int;      };
                    # wallpaperDir        = opt { type = tp.str;      };
                    # wallpaperGamma      = opt { type = tp.float;    };
                    # wallpaperContrast   = opt { type = tp.float;    };
                # windowSpace         = opt { type = tp.int;      };
                # windowBorderWidth   = opt { type = tp.int;      };
                # terminalOpacity     = opt { type = tp.float;    };
                # terminalPadding     = opt { type = tp.attrs;    };
                # barHeight           = opt { type = tp.int;      };
                # magnifiedScale      = opt { type = tp.float;    };

                usePackageList      = true;
                scratchpadWidth     = "2 % 3";
                scratchpadHeight    = "2 % 3";
                font                = "Hack";
            };
        };
    };
}
