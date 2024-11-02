{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        fontsize = 10;
        fontsizeXmobar = 10;
        fontsizeWaybar = 10;

        wallpaperDir = "Landscapes";
        wallpaperGamma = 1.0;
        wallpaperContrast = 1.0;

        windowSpace = 14;
        windowBorderWidth = 0;

        terminalOpacity = 1.0;
        terminalPadding = { x = 0; y = 0; };

        barHeight = 35;

        magnifiedScale = 1.5;

        scratchpadWidth = "2 % 3";
        scratchpadHeight = "2 % 3";
        font = "Hack";
    };
}
