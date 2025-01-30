{ pkgs, ... }:

{
    environment.variables = {
        PCTYPE = "laptop";
        MUTTER_DEBUG_KMS_THREAD_TYPE="user";
    };

    users.groups.ai-user = { };
    users.users.ai-user = {
        isSystemUser = true;
        description = "User for AI interface";
        group = "ai-user";
        extraGroups = [
            "networkmanager"
            "sys"
            "wheel"
        ];
        homeMode = "0711";
        home = "/home/ai-user";
        shell = "${pkgs.zsh}/bin/zsh";
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
            loadModels = [
                "codestral:22b"
                "dolphincoder:15b"
                "aya:8b"
                "llama2-uncensored:7b"
                "codellama:34b"
                "deepseek-coder-v2:16b"
                "llama3.2:1b"
                "codegemma:7b"
                "gemma:7b"
            ];
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
        logind.extraConfig = ''
            HandleLidSwitch=ignore
            HandleLidSwitchExternalPower=ignore
        '';
        keyd = {
            enable = true;
            keyboards.default.settings = {
                main = {
                    kp2 = "down";
                    kp4 = "left";
                    kp6 = "right";
                    kp8 = "up";
                    kp1 = "end";
                    kp3 = "pagedown";
                    kp7 = "home";
                    kp9 = "pageup";
                    kpenter = "enter";
                    kp5 = "down";
                    rightalt = "leftmeta";
                };
            };
        };
        displayManager.autoLogin = {
            enable = true;
            user = "ramak";
        };
    };

    time.timeZone = "Asia/Hong_Kong";

    fonts.packages = with pkgs; [
        hack-font
        noto-fonts
        kochi-substitute
        nerdfonts
    ];
}
