{ pkgs, ... }:

{
    environment.variables = {
        PCTYPE = "laptop";
        MUTTER_DEBUG_KMS_THREAD_TYPE="user";
    };

    environment.systemPackages = with pkgs; [
        light
        brightnessctl
    ];

    services = {
        libinput = {
            enable = true;
            touchpad = {
                naturalScrolling = true;
                tapping = false;
                clickMethod = "clickfinger";
                disableWhileTyping = true;
            };
        };
        ollama = {
            enable = true;
            acceleration = "cuda";
            # models = "/home/ramak/media/models";
        };
        upower.enable = true;
        thermald.enable = true;
        auto-cpufreq = {
            enable = true;
            settings = {
                battery = {
                    governor = "powersave";
                    turbo = "never";
                };
                charger = {
                    governor = "performance";
                    turbo = "auto";
                };
            };
        };
        syncthing = {
            enable = true;
            user = "ramak";
            dataDir = "/home/ramak/dls";
            configDir = "/home/ramak/.config/syncthing";
            settings = {
                devices = {
                    "desktop" = { id = "BWFUHH5-FMJJFJO-JNDOMDV-LMIWAV2-QIJV7Y7-ZTUEPIE-V2BVDXT-QUSKLAL"; };
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
                    "sandbox" = {
                        path = "~/projects/sandbox";
                        ignorePerms = false;
                        devices = [ "desktop" ];
                    };
                };
            };
        };
    };

    time.timeZone = "Asia/Hong_Kong";
}
