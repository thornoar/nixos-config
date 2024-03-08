{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        wallpaperDir = "Landscapes";
        fontsize = 10;
        fontsizeBar = 14;
        windowSpace = lib.mkForce 0;
        padding = { x = 3; y = 3; };
        barheight = lib.mkForce 36;
        xmobarExtraCommands = lib.mkForce ''
            Run Battery [
                "--template" , "<acstatus>",
                "--Low"      , "10",        -- units: %
                "--High"     , "80",        -- units: %
                "--low"      , "red",
                "--normal"   , "orange",
                "--high"     , "green",
                "--",
                "-o", "<left>% (<timeleft>)",
                "-O", "<fc=${config.colorYellow1}>Charging</fc>",
                "-i", "<fc=${config.colorGreen0}>Charged</fc>"
            ] 50,
        '';
        # xmobarExtraOptions = ''
        #     alpha = ${builtins.toString (builtins.floor (255*config.windowOpacity))},
        # '';
        xmobarTemplate = lib.mkForce " %XMonadLog% }{ %kbd% | %date% | %battery% | %alsa:default:Master% ";


        # home.file.".Xmodmap".text = ''
        #     remove mod1 = Alt_R
        #
        #     clear mod4
        #     keycode 108 = Super_R
        #     add mod4 = Super_R
        # '';

        home.packages = with pkgs; [
            light
            xorg.xmodmap
            brightnessctl
        ];
    };
}
