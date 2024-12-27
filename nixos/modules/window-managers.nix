{ pkgs, pkgs-unstable, lib, ... }:
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

    fonts.packages = with pkgs; [
        hack-font
        noto-fonts
        kochi-substitute
        nerdfonts
    ];

    specialisation = {
        xmonad.configuration = {
            boot.loader.systemd-boot.sortKey = "aab";
            environment.variables = {
                SPECIALISATION = lib.mkForce "xmonad";
                TERMINAL = "alacritty";
                XCURSOR_SIZE = "16";
                BROWSER = "firefox -P xmonad";
            };
            services.xserver.dpi = 192;
            services.xserver = {
                # videoDrivers = [ "nvidia" ];
                # deviceSection = ''
                #     Option "DRI" "2"
                #     Option "TearFree" "true"
                # '';
                enable = true;
                xkb.layout = "us";
                xkb.variant = "";
                windowManager.xmonad = {
                    enable = true;
                    enableContribAndExtras = true;
                    haskellPackages = pkgs-unstable.haskellPackages;
                };
                displayManager = {
                    sessionCommands = ''
                        # nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
                        setxkbmap -layout us,ru,de
                        xset -dpms
                        setterm -blank 0 -powerdown 0
                        xset r rate 200 30
                        xset s off
                        transmission-daemon
                        setxkbmap -option grp:alt_shift_toggle us,ru
                    '';
                };
            };
            services = {
                picom = {
                    enable = true;
                    backend = "glx";
                };
                unclutter-xfixes = {
                    enable = true;
                    timeout = 1;
                };
            };
            environment.systemPackages = with pkgs; [       
                xclip
                xsel
                hsetroot
                xvkbd
                wl-clipboard
                keynav
                xcolor
                xkb-switch
            ];
        };

        hyprland.configuration = {
            boot.loader.systemd-boot.sortKey = "aaa";
            environment.variables.SPECIALISATION = lib.mkForce "hyprland";
            programs.hyprland = {
                enable = true;
                package = pkgs.hyprland;
                xwayland.enable = true;
            };
            services = {
                picom = {
                    enable = true;
                    backend = "glx";
                };
                displayManager = {
                    sddm.enable = true;
                    sddm.wayland.enable = true;
                };
            };
            environment.sessionVariables = {
                WLR_NO_HARDWARE_CURSORS = "1";
                CURSOR_INACTIVE_TIMEOUT = "1";
                NIXOS_OZONE_WL = "1";
                HYPRCURSOR_SIZE = "16";
                TERMINAL = "kitty";
                XCURSOR_SIZE = "16";
                BROWSER = "firefox -P hyprland";
            };
            hardware = {
                graphics.enable = lib.mkForce true;
                nvidia.modesetting.enable = lib.mkForce true;
            };
            environment.systemPackages = with pkgs; [
                waybar
                wpaperd
                # wofi
                tofi
                wl-clipboard
                xsel
                warpd
                hyprpicker
                hyprshot
                glib
            ];
        };
    };
}
