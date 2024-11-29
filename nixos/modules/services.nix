{ ... }:
{
    services = {
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
        syncthing = {
            enable = true;
            user = "ramak";
            dataDir = "/home/ramak/dls";
            configDir = "/home/ramak/.config/syncthing";
        };
    };
}
