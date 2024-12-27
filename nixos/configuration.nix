{ sysname, inputs, pkgs, ... }:

{
    config = {
        users.users.ramak = {
            isNormalUser = true;
            description = "Roman Maksimovich";
            extraGroups = [
                "networkmanager"
                "keyd"
                "wheel"
                "sys"
                "root"
                "audio"
                "sound"
                "video"
                "networkmanager"
                "libvirtd"
            ];
            homeMode = "0711";
        };
    
        nix = {
            nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
            settings.experimental-features = [ "nix-command" "flakes" ];
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

        environment = {
            variables = rec {
                NIXPKGS_ALLOW_UNFREE = "1";
                NIXPKGS_ALLOW_BROKEN = "1";
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
                TEXINPUTS = ".:${XDG_DATA_HOME}/latex:$TEXINPUTS";
                EDITOR = "nvim-client";
                VISUAL = "${EDITOR}";
                HISTCONTROL = "ignoreboth";
                READER = "zathura";
                FILEMANAGER = "br";
                IPYTHONDIR="${XDG_CONFIG_HOME}/ipython";
                CARGO_HOME="${XDG_DATA_HOME}/cargo";
                LESSHISTFILE="${XDG_CACHE_HOME}/less/history";
                CUDA_CACHE_PATH="${XDG_CACHE_HOME}/nv";
                XCOMPOSECACHE="${XDG_CACHE_HOME}/X11/xcompose";
                ASYMPTOTE_PDFVIEWER = "$HOME/.nix-profile/bin/zathura";
                SPECIALISATION = "default";
            };
            systemPackages = with pkgs; [
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
                unrar
                p7zip
                sysstat
                man-pages
                man-pages-posix
                scowl
                manix
            ];
            localBinInPath = true;
            wordlist.enable = true;
        };


        services.openssh = {
            enable = true;
            settings = {
                PermitRootLogin = "no";
                PasswordAuthentication = true;
            };
        };

        security.sudo.package = pkgs.sudo.override { withInsults = false; };

        boot.loader.systemd-boot = {
            enable = true;
            configurationLimit = 3;
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

        i18n.defaultLocale = "en_US.UTF-8";

        programs.zsh = {
            enable = true;
            enableCompletion = false;
        };
        users.defaultUserShell = pkgs.zsh;

        documentation.dev.enable = true;

        hardware.enableAllFirmware = true;

        # services.pipewire.enable = false;
        services.pipewire = {
            enable = true;
            alsa = {
                enable = true;
                support32Bit = true;
            };
            audio.enable = true;
            pulse.enable = true;
        };
        # hardware.pulseaudio.enable = true;
        # hardware.pulseaudio.support32Bit = true;
        nixpkgs.config = {
            pulseaudio = true;
            allowUnfree = true;
        };

        virtualisation.libvirtd.enable = true;
        programs = {
            virt-manager.enable = true;
        };

        system.stateVersion = "23.11";
    };
}
