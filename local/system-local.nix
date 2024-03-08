{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu ];

        environment.variables = {
            PCTYPE = "station";
        };

        hardware.opengl = {
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
        };

        services.syncthing.settings = {
            devices = {
                "laptop" = { id = "HJOFBO5-JPN4NRT-PKCJJCE-KPLE7VH-MHKHIGB-Z2SEB4F-J533LPH-C3TTOQV"; };
                "phone" = { id = "RLHSCWU-KTCTYQL-FXTBMN5-CEEH3FB-3TP3B2Y-2T5FE64-SENTOZR-SE5B5QQ"; };
            };
            folders = {
                "music" = {
                    path = "~/media/music";
                    ignorePerms = false;
                    devices = [ "laptop" ];
                };
                "books" = {
                    path = "~/media/books";
                    ignorePerms = false;
                    devices = [ "laptop" ];
                };
                "wallpapers" = {
                    path = "~/media/wallpapers";
                    ignorePerms = false;
                    devices = [ "laptop" ];
                };
            };
        };

        services.xserver = {
            videoDrivers = [ "nvidia" ];
            displayManager = {
                sessionCommands = ''
                    nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
                    setxkbmap -layout us,ru,de
                    xset -dpms
                    setterm -blank 0 -powerdown 0
                    xset r rate 200 30
                    xset s off
                    transmission-daemon
                '';
            };
        };

        fileSystems."/home/ramak/media" = {
            device = "/dev/disk/by-uuid/baf7b861-a1fa-4261-b38f-b7200bee9faf";
            fsType = "ext4";
            options = [ "nofail" "rw" "user" "auto" ];
        };
        # fileSystems."/home/ramak/basic" = {
        #     device = "/dev/disk/by-uuid/334FB4EF0C55A12E";
        #     fsType = "ntfs";
        #     options = [ "nofail" "rw" "user" ];
        # };

        hardware.nvidia = {
            modesetting.enable = true;
            powerManagement.enable = false;
            powerManagement.finegrained = false;
            open = false;
            nvidiaSettings = true;
            package = config.boot.kernelPackages.nvidiaPackages.stable;
        };
    };
}
