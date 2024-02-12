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
        font = mkStrOpt "Hack";

        padding = opt {
            type = tp.attrs;
            default = { x = 4; y = 4; };
        };

        bgColor0 = mkStrOpt "#0b0c0d";#"#0b1012";#"#1e2127";
        bgColor1 = mkStrOpt config.bgColor0;#"#2c3037";
        bgColor2 = mkStrOpt "#43565c";
        fgColor = mkStrOpt "#17a88b";
        brfgColor = mkStrOpt "#00bc96";

        colorBlack = mkStrOpt "#000000";

        colorWhite0 = mkStrOpt "#ffffff";
        colorWhite1 = mkStrOpt "#f8f8f2";
        colorWhite2 = mkStrOpt "#e6efff";
        colorWhite3 = mkStrOpt "#bbbbbb";
        colorWhite4 = mkStrOpt "#6b6b6b";

        colorYellow0 = mkStrOpt "#f1fa8c";
        colorYellow1 = mkStrOpt "#d19a66";
        colorRed0 = mkStrOpt "#ff5555";
        colorRed1 = mkStrOpt "#e06c75";
        colorOrange = mkStrOpt "#ffb86c";

        colorBlue0 = mkStrOpt "#bd93f9";
        colorBlue1 = mkStrOpt "#61afef";
        colorBlue2 = mkStrOpt "#6272a4";
        colorCyan = mkStrOpt "#56b6c2";
        colorGreen0 = mkStrOpt "#89a870";
        colorGreen1 = mkStrOpt "#329c48";

        colorMagenta0 = mkStrOpt "#ff79c6";
        colorMagenta1 = mkStrOpt "#c678dd";

        # xmonadLayouts = mkStrOpt "grid ||| Full ||| spirals ||| magnified ||| tabs";

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
                    Run Date "<fc=${config.colorMagenta0}>%H:%M:%S</fc> | <fc=${config.colorBlue0}>%a %Y-%m-%d</fc>" "date" 10,
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

    # Laptop config

    # config = {
    #     wallpaperDir = "Landscapes";
    #     fontsize = 10;
    #     fontsizeBar = 14;
    #     windowSpace = lib.mkForce 0;
    #     padding = { x = 3; y = 3; };
    #     barheight = lib.mkForce 36;
    #     # xmonadLayouts = "grid ||| Full ||| magnified ||| spirals ||| tabs";
    #     xmobarExtraCommands = lib.mkForce ''
    #         Run Battery [
    #             "--template" , "<acstatus>",
    #             "--Low"      , "10",        -- units: %
    #             "--High"     , "80",        -- units: %
    #             "--low"      , "red",
    #             "--normal"   , "orange",
    #             "--high"     , "green",
    #             "--",
    #             "-o", "<left>% (<timeleft>)",
    #             "-O", "<fc=${config.colorYellow1}>Charging</fc>",
    #             "-i", "<fc=${config.colorGreen0}>Charged</fc>"
    #         ] 50,
    #     '';
    #     # xmobarExtraOptions = ''
    #     #     alpha = ${builtins.toString (builtins.floor (255*config.windowOpacity))},
    #     # '';
    #     xmobarTemplate = lib.mkForce " %XMonadLog% }{ %kbd% | %date% | %battery% | %alsa:default:Master% ";
    #
    #     home.file.".Xmodmap".text = ''
    #         remove mod1 = Alt_R
    #
    #         clear mod4
    #         keycode 108 = Super_R
    #         add mod4 = Super_R
    #     '';
    #
    #     home.packages = with pkgs; [
    #         light
    #         xorg.xmodmap
    #         brightnessctl
    #     ];
    # };
