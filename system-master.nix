{ sysname, inputs, config, lib, pkgs, pkgs-unstable, ... }:

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
                lightdm = {
                    enable = true;
                    greeters.enso.enable = true;
                    # greeters.lomiri.enable = true;
                    greeters.enso.extraConfig = ''
                        font-name=Hack 18
                    '';
                    background = dotfile "lightdm-background.jpg";
                };
            };
            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
                haskellPackages = pkgs-unstable.haskellPackages;
            };
        };

        sound.enable = true;
        hardware.pulseaudio.enable = true;
        hardware.pulseaudio.support32Bit = true;

        nixpkgs.config = {
            pulseaudio = true;
        };

        nixpkgs.config.allowUnfree = true;

        services.unclutter-xfixes = {
            enable = true;
            timeout = 1;
        };

        services.picom = {
            enable = true;
            backend = "glx";
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
        programs.virt-manager.enable = true;
    };
}
