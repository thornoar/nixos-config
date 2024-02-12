{ sysname, usrs, curusrname, inputs, config, lib, pkgs, ... }:

{
    imports = [
        /etc/nixos/hardware-configuration.nix
        /etc/nixos/system-local.nix
    ];

    options = {};

    config = {
        users.users = usrs;
    
        nix.registry = {
            nixpkgs.flake = inputs.nixpkgs;
            ${sysname}.flake = inputs.self;
        };

        environment.variables = rec {
            NIXPKGS_ALLOW_UNFREE = "1";
            XDG_CONFIG_HOME = "$HOME/.config";
            XDG_DATA_HOME = "$HOME/.local/share";
            XDG_STATE_HOME = "$HOME/.local/state";
            XDG_CACHE_HOME = "$HOME/.cache";
            PROJECTS = "$HOME/projects";
            NIXOS_CONFIG = "${PROJECTS}/nixos-config";
            MEDIA = "$HOME/media";
            DE = "generic";
            PAGER = "most";
            NVIM_LISTEN_ADDRESS = "/tmp/nvimsocket";
            TEXINPUTS = ".:${PROJECTS}/libs:$TEXINPUTS";
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
        ];

        services.printing = {
            enable = true;
            drivers = [ pkgs.gutenprint ];
        };

        security.sudo.package = pkgs.sudo.override { withInsults = true; };

        boot.loader.systemd-boot = {
            enable = true;
            configurationLimit = 5;
        };
        boot.loader.timeout = 35996;
        boot.loader.efi.canTouchEfiVariables = true;

        nix.gc = {
            automatic = true;
            dates = "daily";
            options = "--delete-older-than 1d";
        };
        nix.settings.auto-optimise-store = true;

        networking.hostName = sysname;

        networking.networkmanager.enable = true;
        networking.wireless.iwd.enable = true;
        networking.networkmanager.wifi.backend = "iwd";

        time.timeZone = "Europe/Belgrade";
        i18n.defaultLocale = "en_US.UTF-8";

        sound.enable = true;
        hardware.pulseaudio.enable = true;
        hardware.pulseaudio.support32Bit = true;
        nixpkgs.config.pulseaudio = true;

        services.xserver = {
            enable = true;
            xkb.layout = "us";
            xkb.variant = "";
            displayManager.lightdm = {
                enable = true;
                greeters.enso.enable = true;
                background = pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
            };
            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
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
            user = curusrname;
            dataDir = "/home/${user}/dls";
            configDir = "/home/${user}/.config/syncthing";
            # overrideFolders = true;     # overrides any folders added or deleted through the WebUI
            settings = {
                folders = {
                    "music" = {
                        path = "~/media/music";
                    };
                    "books" = {
                        path = "~/media/books";
                        ignorePerms = false;
                    };
                };
            };
        };

        fonts.packages = with pkgs; [
            hack-font
            noto-fonts
            noto-fonts-cjk
            noto-fonts-emoji
            nerdfonts
        ];

        programs.zsh = {
            enable = true;
            enableCompletion = false;
        };
        users.defaultUserShell = pkgs.zsh;

        system.stateVersion = "23.11";

        nix.settings.experimental-features = [ "nix-command" "flakes" ];
    };
}
