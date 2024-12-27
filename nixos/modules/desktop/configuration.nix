{ config, lib, pkgs, modulesPath, ... }:

{
    config = {
        boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu ];

        environment.variables = {
            PCTYPE = "station";
        };

        hardware.graphics = {
            enable = true;
            # driSupport = true;
            # driSupport32Bit = true;
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

        boot.kernelPackages = pkgs.linuxPackages_latest;

        fileSystems."/home/ramak/media" = {
            device = "/dev/disk/by-uuid/baf7b861-a1fa-4261-b38f-b7200bee9faf";
            fsType = "ext4";
            options = [ "nofail" "rw" "user" "auto" ];
        };

        networking.wireless.iwd.enable = true;
        networking.networkmanager.wifi.backend = "iwd";

        services.syncthing.settings = {
            devices = {
                # "laptop" = { id = "TCSGHBY-J7S2EQC-4TZW6ZW-Q7PXKL4-J74ZR37-NJKZDGG-EHEL47Y-OHWZ5A5"; };
                # "phone" = { id = "RLHSCWU-KTCTYQL-FXTBMN5-CEEH3FB-3TP3B2Y-2T5FE64-SENTOZR-SE5B5QQ"; };
            };
            folders = {
                "music" = {
                    path = "~/media/music";
                    ignorePerms = false;
                    devices = [];
                };
                "books" = {
                    path = "~/media/books";
                    ignorePerms = false;
                    devices = [];
                };
                "wallpapers" = {
                    path = "~/media/wallpapers";
                    ignorePerms = false;
                    devices = [];
                };
                "notes" = {
                    path = "~/projects/notes";
                    ignorePerms = false;
                    devices = [];
                };
                "sandbox" = {
                    path = "~/projects/sandbox";
                    ignorePerms = false;
                    devices = [];
                };
            };
        };

        boot.loader.systemd-boot = {
            configurationLimit = 5;
        };

        # hardware.nvidia = {
        #     modesetting.enable = true;
        #     powerManagement.enable = false;
        #     powerManagement.finegrained = false;
        #     open = false;
        #     nvidiaSettings = true;
        #     package = config.boot.kernelPackages.nvidiaPackages.beta;
        # };

        time.timeZone = "Europe/Belgrade";
    };
}
