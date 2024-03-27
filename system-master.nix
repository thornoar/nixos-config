{ sysname, usrname, inputs, config, lib, pkgs, ... }:

{
    imports = [
        /etc/nixos/hardware-configuration.nix
        /etc/nixos/system-local.nix
    ];

    options = {};

    config = {
        users.users."${usrname}" = {
            isNormalUser = true;
            description = usrname;
            extraGroups = [ "networkmanager" "wheel" "sys" "root" "audio" "sound" "video" "networkmanager" ];
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
            MEDIA = "$HOME/media";
            DE = "generic";
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

            (haskellPackages.ghcWithPackages (hpkgs: [ hpkgs.optparse-applicative hpkgs.turtle ]))
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
        boot.supportedFilesystems = [ "ntfs" ];

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
            displayManager = {
                lightdm = {
                    enable = true;
                    # greeters.enso.enable = true;
                    # background = pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
                };
                autoLogin.enable = true;
                autoLogin.user = usrname;
            };
            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
                haskellPackages = pkgs.haskellPackages.extend (
                    pkgs.haskell.lib.packageSourceOverrides {
                        xmonad = pkgs.fetchFromGitHub {
                            owner = "xmonad";
                            repo = "xmonad";
                            rev = "4b9ef5970633490849980f06f6af6dcf76a19cdf";
                            sha256 = "16jl01r2274h4l5m8l6w5dkisamn73969qhs39yagwvyygsr8wjv";
                        };
                        xmonad-contrib = pkgs.fetchFromGitHub {
                            owner = "xmonad";
                            repo = "xmonad-contrib";
                            rev = "bfe2f5b3f9fa89988141604680f3639a81af1f1b";
                            sha256 = "0b5w5gm797jf369bj5rxbhazi3yadj7ax4vqc0fdp4f11mvad6w8";
                        };
                    }
                );
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
            user = usrname;
            dataDir = "/home/${user}/dls";
            configDir = "/home/${user}/.config/syncthing";
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
    };
}
