{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        home.pointerCursor.size = 16;

        xmonad = lib.mkForce {
            desktopScale = 1.6;
            fontsize = 11;
            fontsizeXmobar = 13;
            fontsizeRunPrompt = 11;
            windowSpaceInner = 14;
            windowSpaceOuter = 14;
            windowBorderWidth = 0;
            terminalOpacity = 0.97;
            terminalPaddingX = 4;
            terminalPaddingY = 3;
            barHeight = 35;
            magnifiedScale = 1.5;
            scratchpadWidth = "19 % 30";
            scratchpadHeight = "39 % 60";
            firefoxScale = 1.0;
        };

        hyprland = lib.mkForce {
            desktopScale = 1.6;
            resolution = "2560x1440";
            fontsize = 11;
            fontsizeWaybar = 11;
            windowSpaceInner = 4;
            windowSpaceOuter = 8;
            windowBorderWidth = 0;
            terminalOpacity = 0.9;
            terminalPadding = 1;
            rounding = 5;
            barHeight = 50;
            firefoxScale = 1.8;
        };

        wallpaper = lib.mkForce {
            dir = "Landscapes";
            gamma = 0.9;
            contrast = 1.0;
        };

        misc = lib.mkForce {
            usePackageList = true;   
            systemFont = "Hack";
            monitorName = "HDMI-A-1";
            wmStartupCommand = "echo hi";
        };

        xmobar = lib.mkForce {
            extraCommands = ''
                Run Battery [
                    "--template" , "<acstatus>",
                    "--Low"      , "10",        -- units: %
                    "--High"     , "80",        -- units: %
                    "--low"      , "red",
                    "--normal"   , "orange",
                    "--high"     , "green",
                    "--",
                    "-o", "<left>% (<timeleft>)",
                    "-O", "<fc=${config.colors.colorYellow1}>Charging</fc>",
                    "-i", "<fc=${config.colors.colorGreen0}>Charged</fc>"
                ] 50,
                Run Brightness [
                    "-t", "<fc=${config.colors.colorMagenta1}>BRI: <percent>%</fc>", "--", "-D", "intel_backlight"
                ] 60,
            '';
            extraOptions  = ""; # {
            template = " %XMonadLog% } %alsa:default:Master% | %date% { %bright% | %kbd% | %multicpu% | %memory% | %battery% "; # }
        };
    };
}
