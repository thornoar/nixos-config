{ config, lib, pkgs, modulesPath, ... }:

{
    boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu ];

    environment.variables = {
        PCTYPE = "station";
    };

    hardware.opengl = {
        enable = true;
        driSupport = true;
        driSupport32Bit = true;
    };

    services.xserver = {
        videoDrivers = [ "nvidia" ];
        displayManager = {
            sessionCommands = ''
                nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
                feh --randomize --bg-fill $HOME/wallpapers/SpiderWallpapers
                setxkbmap -layout us,ru,de
                xset -dpms
                setterm -blank 0 -powerdown 0
                xset r rate 200 30
                xset s off
            '';
        };
    };

    fileSystems."/home/ramak/media" = {
        device = "/dev/disk/by-uuid/baf7b861-a1fa-4261-b38f-b7200bee9faf";
        fsType = "ext4";
        options = [ "nofail" "rw" "user" "auto" ];
    };
    fileSystems."/home/ramak/basic" = {
        device = "/dev/disk/by-uuid/334FB4EF0C55A12E";
        fsType = "ntfs";
        options = [ "nofail" "rw" "user" ];
    };

    hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
}
