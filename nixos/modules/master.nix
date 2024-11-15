{ lib, pkgs, config, pkgs-unstable, ... }:

{
    imports = (
        let
            path = /home/ramak/projects/nixos-local-config/system-local.nix;
        in
            if (builtins.pathExists path) then [ path ] else [ ../src/system-template.nix ]
    );

    config = {
        environment.variables.SPECIALISATION = "default";

        services.keyd = {
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
                    # kp5 = "enter";
                    rightalt = "leftmeta";
                };
            };
        };

        specialisation = {
            xmonad.configuration = {
                boot.loader.systemd-boot.sortKey = "aab";
                environment.variables = {
                    SPECIALISATION = lib.mkForce "xmonad";
                    TERMINAL = "alacritty";
                    XCURSOR_SIZE = "24";
                };
                # services.xserver.dpi = 192;
                services.xserver = {
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
                };
                hardware = {
                    opengl.enable = lib.mkForce true;
                    nvidia.modesetting.enable = lib.mkForce true;
                };
                environment.systemPackages = with pkgs; [
                    waybar
                    wpaperd
                    wofi
                    wl-clipboard
                    xsel
                    warpd
                    hyprpicker
                ];
            };
        };

        # services.xserver = {
        #     # videoDrivers = [ "nvidia" ];
        # };
        services = {
            displayManager.autoLogin = {
                enable = true;
                user = "ramak";
            };
        };

        sound.enable = true;
        hardware.pulseaudio.enable = true;
        hardware.pulseaudio.support32Bit = true;
        nixpkgs.config = {
            pulseaudio = true;
            allowUnfree = true;
        };

        services.syncthing = {
            enable = true;
            user = "ramak";
            dataDir = "/home/ramak/dls";
            configDir = "/home/ramak/.config/syncthing";
        };

        fonts.packages = with pkgs; [
            hack-font
            noto-fonts
            kochi-substitute
            nerdfonts
        ];

        virtualisation.libvirtd.enable = true;
        programs = {
            virt-manager.enable = true;
        };

        environment = {
            wordlist.enable = true;
            systemPackages = with pkgs; [
                scowl
                manix
            ];
        };

        services.logind.extraConfig = ''
            HandleLidSwitch=ignore
            HandleLidSwitchExternalPower=ignore
        '';
    };
}
