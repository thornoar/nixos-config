{ config, lib, pkgs, modulesPath, ... }:

{
    home.pointerCursor.size = 16;

    xmonad = lib.mkForce {
        desktopScale = 1.0;
        fontsize = 10;
        fontsizeXmobar = 44;
        fontsizeRunPrompt = 22;
        windowSpaceInner = 4;
        windowSpaceOuter = 8;
        windowBorderWidth = 0;
        terminalOpacity = 0.9;
        terminalPaddingX = 3;
        terminalPaddingY = 3;
        barHeight = 100;
        magnifiedScale = 1.5;
        scratchpadWidth = "4 % 5";
        scratchpadHeight = "35 % 50";
        firefoxScale = 1.8;
    };

    hyprland = lib.mkForce {
        desktopScale = 2.0;
        widthPixels = 2880;
        heightPixels = 1620;
        resolution = "2880x1620";
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
        dir = "Frieren";
        gamma = 0.9;
        contrast = 1.0;
    };

    misc = lib.mkForce {
        usePackageList = true;   
        systemFont = "Hack";
        monitorName = "eDP-1";
        wmStartupCommand = "sleep 8; echo disable > /sys/firmware/acpi/interrupts/gpe6F; sleep 1; echo disable > /sys/firmware/acpi/interrupts/gpe6F; sleep 1; echo disable > /sys/firmware/acpi/interrupts/gpe6F";
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
}
