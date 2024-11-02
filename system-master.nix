{ lib, pkgs, pkgs-unstable, ... }:

{
    imports = (
        let path = /home/ramak/projects/nixos-local-config/system-local.nix;
        in if (builtins.pathExists path) then [ path ] else [ ./dotfiles/system-template.nix ]
    ) ++ [
        ./system-minimal.nix
        /etc/nixos/hardware-configuration.nix
    ];

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
                };
            };
        };

        specialisation = {
            xmonad.configuration = {
                boot.loader.systemd-boot.sortKey = "aab";
                environment.variables = {
                    SPECIALISATION = lib.mkForce "xmonad";
                    TERMINAL = "alacritty";
                };
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
                    HYPRCURSOR_SIZE = "32";
                    TERMINAL = "kitty";
                };
                hardware = {
                    opengl.enable = lib.mkForce true;
                    nvidia.modesetting.enable = lib.mkForce true;
                };
                environment.systemPackages = with pkgs; [
                    waybar
                    hyprpaper
                    wofi
                    wl-clipboard
                    xsel
                    warpd
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
    };
}
