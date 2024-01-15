{ lib, pkgs, config, ... }:
{
    options = 
    let opt = lib.mkOption; tp = lib.types; in
    {
        fontsize = opt {
            type = tp.int;
        };
        fontsizeBar = opt {
            type = tp.int;
        };
        wallpaperDir = opt {
            type = tp.str;
            default = "Landscapes";
        };
        windowSpace = opt {
            type = tp.int;
            default = 5;
        };
        bgColor = opt {
            type = tp.str;
            default = "#1e2127";
            # default = "#000000";
        };
        fgColor = opt {
            type = tp.str;
            default = "#17a88b";
        };
        brfgColor = opt {
            type = tp.str;
            default = "#00bc96";
        };
        colorWhite = opt {
            type = tp.str;
            default = "#f8f8f2";
        };
        colorWhite_alt = opt {
            type = tp.str;
            default = "#bbbbbb";
        };
        colorBlue = opt {
            type = tp.str;
            default = "#bd93f9" ;
        };
        colorBlue_alt = opt {
            type = tp.str;
            default = "#61afef";
        };
        colorMagenta = opt {
            type = tp.str;
            default = "#ff79c6";
        };
        colorMagenta_alt = opt {
            type = tp.str;
            default = "#c678dd";
            # default = config.colorBlue;
        };
        font = opt {
            type = tp.str;
            default = "Hack";
        };
        padding = opt {
            type = tp.attrs;
            default = { x = 6; y = 6; };
        };
        barheight = opt {
            type = tp.int;
            default = 35;
        };
        xmonadLayouts = opt {
            type = tp.str;
            default = "tall ||| Full ||| tabs ||| magnified ||| spirals ||| accordion";
        };
        xmobarOptions = opt {
            type = tp.str;
            default = ''
                Config { font     = "xft:${config.font} Nerd Font Mono-${builtins.toString config.fontsizeBar}"
                       , bgColor  = "${config.bgColor}"
                       , fgColor  = "${config.fgColor}"
                       , position = TopH ${builtins.toString config.barheight}
                       , persistent = False
                       , hideOnStart = False
                       , allDesktops = True
                       , lowerOnStart = True
                       , commands = [
                                    Run Alsa "default" "Master"
                                        [ "--template", "<fc=${config.colorWhite}><volumestatus></fc>"
                                        , "--suffix"  , "True"
                                        , "--"
                                        , "--on", ""
                                    ]
                                    , Run Date "<fc=${config.colorMagenta}>%H:%M:%S</fc> | <fc=${config.colorBlue}>%a %Y-%m-%d</fc>" "date" 10
                                    , Run XMonadLog
                                    , Run Kbd [ ("us", "<fc=${config.colorWhite}>US</fc>")
                                                , ("ru", "<fc=${config.colorWhite}>RU</fc>")
                                                , ("de", "<fc=${config.colorWhite}>DE</fc>")
                                    ]
                       ]
                       , sepChar  = "%"
                       , alignSep = "}{"
                       , template = " %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% "
                       }
            '';
        };
    };
}
