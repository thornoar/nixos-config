{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        environment.variables = {
            PCTYPE = "laptop";
            MUTTER_DEBUG_KMS_THREAD_TYPE="user";
        };

        services.xserver = {
            videoDrivers = [ "nvidiaLegacy390" ];
            # videoDrivers = [ "nvidia" ];
            displayManager = {
                sessionCommands = ''
                    # nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
                    setxkbmap -layout us,ru,de
                    xset -dpms
                    setterm -blank 0 -powerdown 0
                    xset r rate 200 30
                    xset s off
                    transmission-daemon
                '';
            };
            libinput = {
                enable = true;
                touchpad = {
                    naturalScrolling = true;
                    tapping = true;
                    clickMethod = "clickfinger";
                    disableWhileTyping = true;
                };
            };
        };

        fileSystems."/home/ramak/media" = {
            device = "/dev/disk/by-uuid/d365c266-1fdd-42b1-a576-e7e9efd3e53f";
            fsType = "ext4";
            options = [ "nofail" "rw" "user" "auto" ];
        };
        # fileSystems."/home/ramak/basic" = {
        #     device = "/dev/disk/by-uuid/334FB4EF0C55A12E";
        #     fsType = "ntfs";
        #     options = [ "nofail" "rw" "user" ];
        # };

        services.syncthing.settings = {
            devices = {
                "station" = { id = "QI6LC7P-7I62ZQ3-X6SBRVR-ECVBFTY-WCJ4555-RQU5B3Z-JMXUXDK-TSLBGA4"; };
                "phone" = { id = "RLHSCWU-KTCTYQL-FXTBMN5-CEEH3FB-3TP3B2Y-2T5FE64-SENTOZR-SE5B5QQ"; };
            };
            folders = {
                "music" = {
                    path = "~/media/music";
                    ignorePerms = false;
                    devices = [ "station" ];
                };
                "books" = {
                    path = "~/media/books";
                    ignorePerms = false;
                    devices = [ "station" ];
                };
                "wallpapers" = {
                    path = "~/media/wallpapers";
                    ignorePerms = false;
                    devices = [ "station" ];
                };
            };
        };

        # hardware.opengl = {
        #     enable = true;
        #     driSupport = true;
        #     driSupport32Bit = true;
        # };
        # hardware.nvidia = {
        #     modesetting.enable = true;
        #     powerManagement.enable = false;
        #     powerManagement.finegrained = false;
        #     nvidiaSettings = true;
        #     forceFullCompositionPipeline = true;
        #     open = false;
        #     # package = config.boot.kernelPackages.nvidiaPackages.legacy_390;
        #     package = config.boot.kernelPackages.nvidiaPackages.stable;
        # };
        hardware.nvidia = {
            forceFullCompositionPipeline = true;
        };
        boot.blacklistedKernelModules = [ "nouveau" ];
    };
}
