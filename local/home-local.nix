{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        fontsizeBar = 13;
        fontsize = 11;
        wallpaperDir = "Landscapes";
        scratchpadHeight = lib.mkForce "19 % 30";
        magnifiedScale = 1.635;
    };
}
