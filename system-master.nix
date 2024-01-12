{ config, pkgs, ... }:

{
    environment.variables = rec {
        NIXPKGS_ALLOW_UNFREE = "1";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_STATE_HOME = "$HOME/.local/state";
        XDG_CACHE_HOME = "$HOME/.cache";
        NIXOS_CONFIG = "$HOME/nixos-config";
        PROJECTS = "$HOME/projects";
        MEDIA = "$HOME/media";
        DE = "generic";
        NVIM_LISTEN_ADDRESS = "/tmp/nvimsocket";
        TEXINPUTS = ".:$PROJECTS/libs:$TEXINPUTS";
        EDITOR="nvim";
        TERMINAL="alacritty";
        BROWSER="firefox";
        READER="zathura";
        FILEMANAGER = "ranger";
    };

    imports = [
        ./specifics/nixos/hardware-configuration.nix
        ./specifics/optional-configuration.nix
    ];

    boot.loader.systemd-boot = {
        enable = true;
        configurationLimit = 2;
    };
    boot.loader.timeout = 35996;
    boot.loader.efi.canTouchEfiVariables = true;

    nix.gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 1d";
    };
    nix.settings.auto-optimise-store = true;

    networking.hostName = "nixos"; # Define your hostname.

    # Enable networking
    networking.networkmanager.enable = true;
    networking.wireless.iwd.enable = true;
    networking.networkmanager.wifi.backend = "iwd";

    # Set your time zone.
    time.timeZone = "Europe/Belgrade";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";

    sound.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;
    nixpkgs.config.pulseaudio = true;

    programs.light.enable = true;

    # Configure keymap in X11
    services.xserver = {
        enable = true;
        layout = "us";
        xkbVariant = "";
        displayManager = {
            lightdm.enable = true;
            lightdm.greeters.enso.enable = true;
        };
        windowManager.xmonad = {
            enable = true;
            enableContribAndExtras = true;
        };
    };

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.ramak = {
        isNormalUser = true;
        description = "Roman Maksimovich";
        extraGroups = [ "networkmanager" "wheel" "sys" "root" "audio" "sound" "video" "networkmanager" ];
        packages = with pkgs; [];
    };

    # Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    # List packages installed in system profile. To search, run:
    environment.systemPackages = with pkgs; [
        home-manager

        # system tools
        nix-du
        sysstat
        lm_sensors
        ethtool
        vim
        wget
        curl
        usbutils
        pciutils
        gcc
        git
        lshw

        # archives
        zip
        xz
        unzip
        p7zip

        # utils
        ripgrep
        xclip
        xsel
        eza
        fzf
    ];

    services.unclutter = {
        enable = true;
        timeout = 1;
    };

    fonts.packages = with pkgs; [
        hack-font
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        nerdfonts
    ];

    programs.zsh.enable = true;
    users.defaultUserShell = pkgs.zsh;

    system.stateVersion = "23.11";

    nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
