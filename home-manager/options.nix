{ lib, pkgs, config, ... }:
{
    options = 
    let
        opt = lib.mkOption;
        tp = lib.types;
        ts = builtins.toString;
    in
    {
        fontsize            = opt { type = tp.int;      };
        fontsizeBar         = opt { type = tp.int;      };
        wallpaperDir        = opt { type = tp.str;      };
        wallpaperGamma      = opt { type = tp.float;    };
        wallpaperContrast   = opt { type = tp.float;    };
        windowSpace         = opt { type = tp.int;      };
        windowBorderWidth   = opt { type = tp.int;      };
        terminalOpacity     = opt { type = tp.float;    };
        terminalPadding     = opt { type = tp.attrs;    };
        barHeight           = opt { type = tp.int;      };
        magnifiedScale      = opt { type = tp.float;    };
        scratchpadWidth     = opt { type = tp.str;      default = "2 % 3";          };
        scratchpadHeight    = opt { type = tp.str;      default = "2 % 3";          };
        font                = opt { type = tp.str;      default = "Hack";           };

        bgColor0            = opt { type = tp.str;      default = "#0b0c0d";        };
        bgColor1            = opt { type = tp.str;      default = "#2c3037";        };
        bgColor2            = opt { type = tp.str;      default = "#43565c";        };
        fgColor             = opt { type = tp.str;      default = "#17a88b";        };
        brfgColor           = opt { type = tp.str;      default = "#00bc96";        };

        colorBlack          = opt { type = tp.str;      default = "#000000";        };

        colorWhite0         = opt { type = tp.str;      default = "#ffffff";        };
        colorWhite1         = opt { type = tp.str;      default = "#f8f8f2";        };
        colorWhite2         = opt { type = tp.str;      default = "#e6efff";        };
        colorWhite3         = opt { type = tp.str;      default = "#bbbbbb";        };
        colorWhite4         = opt { type = tp.str;      default = "#595f6b";        };

        colorYellow0        = opt { type = tp.str;      default = "#f1fa8c";        };
        colorYellow1        = opt { type = tp.str;      default = "#d19a66";        };
        colorYellow2        = opt { type = tp.str;      default = "#eebb9e";        };

        colorOrange0        = opt { type = tp.str;      default = "#ffb86c";        };
        colorOrange1        = opt { type = tp.str;      default = "#c28141";        };
        colorOrange2        = opt { type = tp.str;      default = "#d27141";        };

        colorRed0           = opt { type = tp.str;      default = "#ff5555";        };
        colorRed1           = opt { type = tp.str;      default = "#e86671";        };
        colorRed2           = opt { type = tp.str;      default = "#dd9999";        };

        colorBlue0          = opt { type = tp.str;      default = "#3070f0";        };
        colorBlue1          = opt { type = tp.str;      default = "#61afef";        };
        colorBlue2          = opt { type = tp.str;      default = "#6272a4";        };

        colorCyan           = opt { type = tp.str;      default = "#56b6c2";        };

        colorGreen0         = opt { type = tp.str;      default = "#89a870";        };
        colorGreen1         = opt { type = tp.str;      default = "#329c48";        };
        colorGreen2         = opt { type = tp.str;      default = "#78971a";        };
        colorGreen4         = opt { type = tp.str;      default = "#98c379";        };

        colorMagenta0       = opt { type = tp.str;      default = "#ff79c6";        };
        colorMagenta1       = opt { type = tp.str;      default = "#bd93f9";        };
        colorMagenta2       = opt { type = tp.str;      default = "#c678dd";        };
        colorMagenta3       = opt { type = tp.str;      default = "#b16286";        };

        xmobarExtraCommands = opt { type = tp.str;      default = "";               };
        xmobarExtraOptions  = opt { type = tp.str;      default = "";               };
        xmobarTemplate = opt {
            type = tp.str;
            default = " %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% ";
        };
        # alpha = ${ts (builtins.floor (255*config.terminalOpacity))},
        xmobarOptions = opt {
            type = tp.str;
            default = ''
                Config {
                    font     = "xft:${config.font} Nerd Font Mono-${ts config.fontsizeBar}",
                    bgColor  = "${config.bgColor0}",
                    fgColor  = "${config.fgColor}",
                    position = TopH ${ts config.barHeight},
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
    };
}
