{ lib, pkgs, config, ... }:
{
    options = 
    let
        opt = lib.mkOption;
        tp = lib.types;
        mkOpt = typ: df: opt {
            type = tp.${typ};
            default = df;
        };
    in
    {
        fontsize = opt { type = tp.int; };
        fontsizeBar = opt { type = tp.int; };
        dotfiledir = mkOpt "path" ./dotfiles;
        wallpaperDir = mkOpt "str" "Landscapes";
        windowSpace = mkOpt "int" 5;

        bgColor0 = mkOpt "str" "#0b1012";#"#1e2127";
        bgColor1 = mkOpt "str" "#2c3037";
        bgColor2 = mkOpt "str" "#43565c";
        fgColor = mkOpt "str" "#17a88b";
        brfgColor = mkOpt "str" "#00bc96";

        colorBlack = mkOpt "str" "#000000";

        colorWhite0 = mkOpt "str" "#ffffff";
        colorWhite1 = mkOpt "str" "#f8f8f2";
        colorWhite2 = mkOpt "str" "#e6efff";
        colorWhite3 = mkOpt "str" "#bbbbbb";
        colorWhite4 = mkOpt "str" "#6b6b6b";

        colorYellow0 = mkOpt "str" "#f1fa8c";
        colorYellow1 = mkOpt "str" "#d19a66";
        colorRed0 = mkOpt "str" "#ff5555";
        colorRed1 = mkOpt "str" "#e06c75";
        colorOrange = mkOpt "str" "#ffb86c";

        colorBlue0 = mkOpt "str" "#bd93f9";
        colorBlue1 = mkOpt "str" "#61afef";
        colorBlue2 = mkOpt "str" "#6272a4";
        colorCyan = mkOpt "str" "#56b6c2";
        colorGreen0 = mkOpt "str" "#89a870";
        colorGreen1 = mkOpt "str" "#329c48";

        colorMagenta0 = mkOpt "str" "#ff79c6";
        colorMagenta1 = mkOpt "str" "#c678dd";

        font = mkOpt "str" "Hack";
        padding = mkOpt "attrs" { x = 6; y = 6; };
        barheight = mkOpt "int" 35;
        xmonadLayouts = mkOpt "str" "tall ||| Full ||| tabs ||| magnified ||| spirals";
        xmobarOptions = opt {
            type = tp.str;
            default = ''
                Config { font     = "xft:${config.font} Nerd Font Mono-${builtins.toString config.fontsizeBar}"
                       , bgColor  = "${config.bgColor0}"
                       , fgColor  = "${config.fgColor}"
                       , position = TopH ${builtins.toString config.barheight}
                       , persistent = False
                       , hideOnStart = False
                       , allDesktops = True
                       , lowerOnStart = True
                       , commands = [
                                    Run Alsa "default" "Master"
                                        [ "--template", "<fc=${config.colorWhite1}><volumestatus></fc>"
                                        , "--suffix"  , "True"
                                        , "--"
                                        , "--on", ""
                                    ]
                                    , Run Date "<fc=${config.colorMagenta0}>%H:%M:%S</fc> | <fc=${config.colorBlue0}>%a %Y-%m-%d</fc>" "date" 10
                                    , Run XMonadLog
                                    , Run Kbd [ ("us", "<fc=${config.colorWhite1}>US</fc>")
                                                , ("ru", "<fc=${config.colorWhite1}>RU</fc>")
                                                , ("de", "<fc=${config.colorWhite1}>DE</fc>")
                                    ]
                       ]
                       , sepChar  = "%"
                       , alignSep = "}{"
                       , template = " | %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% | "
                       }
            '';
        };
    };
}
