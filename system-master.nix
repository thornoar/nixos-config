{ lib, pkgs, pkgs-unstable, pkgs-old, ... }:

{
    imports = (
        let path = /home/ramak/projects/nixos-local-config/system-local.nix;
        in if (builtins.pathExists path) then [ path ] else [ ./dotfiles/system-template.nix ]
    ) ++ [
        ./system-minimal.nix
        /etc/nixos/hardware-configuration.nix
    ];

    options = {};

    config = 
    let 
        dotfile = str: lib.path.append ./dotfiles str;
    in {
        services.xserver = {
            displayManager = {
                # lightdm = {
                #     enable = true;
                #     greeters.enso.enable = true;
                #     background = dotfile "lightdm-background.jpg";
                # };
            };
            enable = true;
            xkb.layout = "us";
            xkb.variant = "";
            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
                haskellPackages = pkgs-unstable.haskellPackages;
            };
        };
        services = {
            displayManager = {
                sddm = {
                    enable = true;
                    wayland.enable = true;
                };
            };
            picom = {
                enable = true;
                backend = "glx";
            };
            unclutter-xfixes = {
                enable = true;
                timeout = 1;
            };
        };

        programs.hyprland = {
            enable = true;
            package = pkgs.hyprland;
            xwayland.enable = true;
        };
        environment.sessionVariables = {
            WLR_NO_HARDWARE_CURSORS = "1";
            CURSOR_INACTIVE_TIMEOUT = "1";
            NIXOS_OZONE_WL = "1";
            HYPRCURSOR_SIZE = "32";
        };
        hardware = {
            opengl.enable = lib.mkForce true;
            nvidia.modesetting.enable = lib.mkForce true;
        };
        environment.systemPackages = with pkgs; [
            waybar
            hyprpaper
            wofi
        ];

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
