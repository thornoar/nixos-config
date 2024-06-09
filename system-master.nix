{ sysname, inputs, config, lib, pkgs, pkgs-unstable, ... }:

{
    imports = (
        let
            path = /home/ramak/projects/nixos-local-config/system-local.nix;
        in if (builtins.pathExists path) then [ path ] else [ ./dotfiles/system-template.nix ]
    ) ++ [
        /etc/nixos/hardware-configuration.nix
    ];

    options = {};

    config = {
        users.users.ramak = {
            isNormalUser = true;
            description = "Roman Maksimovich";
            extraGroups = [
                "networkmanager"
                "wheel"
                "sys"
                "root"
                "audio"
                "sound"
                "video"
                "networkmanager"
                "libvirtd"
            ];
            packages = with pkgs; [];
        };
    
        nix = {
            settings.experimental-features = [ "nix-command" "flakes" "repl-flake" ];
            extraOptions = ''
                warn-dirty = false
            '';
            registry = {
                nixpkgs.flake = inputs.nixpkgs;
                ${sysname}.flake = inputs.self;
            };
        };

        programs.nix-index = {
            enable = true;
            enableZshIntegration = true;
        };
        programs.command-not-found.enable = false;

        environment.variables = rec {
            NIXPKGS_ALLOW_UNFREE = "1";
            XDG_CONFIG_HOME = "$HOME/.config";
            XDG_DATA_HOME = "$HOME/.local/share";
            XDG_STATE_HOME = "$HOME/.local/state";
            XDG_CACHE_HOME = "$HOME/.cache";
            PROJECTS = "$HOME/projects";
            NIXOS_CONFIG = "${PROJECTS}/nixos-config";
            NIXOS_LOCAL = "${PROJECTS}/nixos-local-config";
            MEDIA = "$HOME/media";
            DE = "generic";
            NVIM_LISTEN_ADDRESS = "/tmp/nvimsocket";
            TEXINPUTS = ".:${PROJECTS}/LaTeX-libraries:$TEXINPUTS";
            EDITOR = "nvim";
            VISUAL = "${EDITOR}";
            TERMINAL = "alacritty";
            HISTCONTROL = "ignoreboth";
            BROWSER = "firefox";
            READER = "zathura";
            FILEMANAGER = "br";
            IPYTHONDIR="${XDG_CONFIG_HOME}/ipython";
            CARGO_HOME="${XDG_DATA_HOME}/cargo";
            LESSHISTFILE="${XDG_CACHE_HOME}/less/history";
            CUDA_CACHE_PATH="${XDG_CACHE_HOME}/nv";
            XCOMPOSECACHE="${XDG_CACHE_HOME}/X11/xcompose";
            ASYMPTOTE_PDFVIEWER = "$HOME/.nix-profile/bin/zathura";
        };
        environment.systemPackages = with pkgs; [
            home-manager
            vim
            wget
            curl
            usbutils
            pciutils
            gcc
            git
            lshw
            zip
            xz
            unzip
            p7zip

            # (haskellPackages.ghcWithPackages (hpkgs: [ hpkgs.optparse-applicative hpkgs.turtle ]))
        ];
        environment.localBinInPath = true;

        services.openssh = {
            enable = true;
            settings = {
                PermitRootLogin = "no";
                PasswordAuthentication = true;
            };
        };

        security.sudo.package = pkgs.sudo.override { withInsults = true; };

        boot.loader.systemd-boot = {
            enable = true;
            configurationLimit = 5;
        };
        boot.loader.timeout = 35996;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.supportedFilesystems = [ "ntfs" ];

        nix.gc = {
            automatic = true;
            dates = "daily";
            options = "--delete-older-than 1d";
        };
        nix.settings.auto-optimise-store = true;

        networking.hostName = sysname;
        networking.networkmanager.enable = true;

        time.timeZone = "Europe/Belgrade";
        i18n.defaultLocale = "en_US.UTF-8";

        sound.enable = true;
        hardware.pulseaudio.enable = true;
        hardware.pulseaudio.support32Bit = true;
        nixpkgs.config.pulseaudio = true;

        hardware.bluetooth = {
            enable = true;
            powerOnBoot = true;
            settings = {
                General.Experimental = true;
            };
        };

        services.xserver = {
            enable = true;
            xkb.layout = "us";
            xkb.variant = "";
            displayManager = {
                lightdm = {
                    enable = true;
                    # greeters.enso.enable = true;
                    # background = pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
                };
                autoLogin.enable = true;
                autoLogin.user = "ramak";
            };
            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
                haskellPackages = pkgs-unstable.haskellPackages;
            };
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

        services.syncthing = rec {
            enable = true;
            user = "ramak";
            dataDir = "/home/${user}/dls";
            configDir = "/home/${user}/.config/syncthing";
        };

        fonts.packages = with pkgs; [
            hack-font
            noto-fonts
            kochi-substitute
            nerdfonts
        ];

        programs.zsh = {
            enable = true;
            enableCompletion = false;
        };
        users.defaultUserShell = pkgs.zsh;

        virtualisation.libvirtd.enable = true;
        programs.virt-manager.enable = true;

        system.stateVersion = "23.11";
    };
}
