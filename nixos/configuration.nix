{ sysname, pkgs, ... }:

{
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
      "libvirtd"
      "adbusers"
    ];
    homeMode = "0711";
  };

  nix = {
    nixPath = [ "nixpkgs=${pkgs.inputs.nixpkgs}" ];
    settings = {
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };
    extraOptions = ''
      warn-dirty = false
    '';
    registry = {
      nixpkgs.flake = pkgs.inputs.nixpkgs;
      ${sysname}.flake = pkgs.inputs.self;
    };
  };

  programs.nix-index = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.command-not-found.enable = false;
  programs.adb.enable = true;

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
      MEDIA = "$HOME/media";
      DE = "generic";
      NVIM_LISTEN_ADDRESS = "/tmp/nvimsocket";
      TEXINPUTS = ".:${XDG_DATA_HOME}/latex:$TEXINPUTS";
      EDITOR = "nvim-client";
      VISUAL = "${EDITOR}";
      HISTCONTROL = "ignoreboth";
      READER = "zathura";
      FILEMANAGER = "br";
      IPYTHONDIR = "${XDG_CONFIG_HOME}/ipython";
      CARGO_HOME = "${XDG_DATA_HOME}/cargo";
      LESSHISTFILE = "${XDG_CACHE_HOME}/less/history";
      CUDA_CACHE_PATH = "${XDG_CACHE_HOME}/nv";
      XCOMPOSECACHE = "${XDG_CACHE_HOME}/X11/xcompose";
      ASYMPTOTE_PDFVIEWER = "$HOME/.nix-profile/bin/zathura";
      SPECIALISATION = "default";
    };
    systemPackages = pkgs.readPackages ./src/packages/general.txt pkgs;
    localBinInPath = true;
    wordlist.enable = true;
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = true;
      PubkeyAuthentication = true;
    };
  };

  services.openvpn.servers = let
    createConfig = name: {
      config = "config /root/nixos/openvpn/${name}.ovpn";
      updateResolvConf = true;
      autoStart = false;
    };
  in {
    server-us-2-protonvpn = createConfig "us-free-2.protonvpn.udp";
    server-nl-2-protonvpn = createConfig "nl-free-2.protonvpn.udp";
    server-jp-2-protonvpn = createConfig "jp-free-2.protonvpn.udp";
    server-de-1-freeopenvpn = createConfig "de-free-1.freeopenvpn.tcp";
    server-de-2-freeopenvpn = createConfig "de-free-2.freeopenvpn.udp";
    server-kr-1-freeopenvpn = createConfig "kr-free-1.freeopenvpn.tcp";
    server-kr-2-freeopenvpn = createConfig "kr-free-2.freeopenvpn.udp";
    server-kr-3-freeopenvpn = createConfig "kr-free-3.freeopenvpn.udp";
    server-ru-2-freeopenvpn = createConfig "ru-free-2.freeopenvpn.udp";
    server-th-2-freeopenvpn = createConfig "th-free-2.freeopenvpn.udp";
    server-us-1-freeopenvpn = createConfig "us-free-1.freeopenvpn.tcp";
    server-us-3-freeopenvpn = createConfig "us-free-3.freeopenvpn.udp";
    server-de-3-vpnbook = createConfig "de-free-3.vpnbook.tcp";
    server-de-4-vpnbook = createConfig "de-free-4.vpnbook.udp";
    server-de-5-vpnbook = createConfig "de-free-5.vpnbook.tcp";
    server-de-6-vpnbook = createConfig "de-free-6.vpnbook.udp";
    server-ca-1-vpnbook = createConfig "ca-free-1.vpnbook.tcp";
    server-ca-2-vpnbook = createConfig "ca-free-2.vpnbook.udp";
    server-ca-3-vpnbook = createConfig "ca-free-3.vpnbook.tcp";
    server-ca-4-vpnbook = createConfig "ca-free-4.vpnbook.udp";
    server-fr-1-vpnbook = createConfig "fr-free-1.vpnbook.tcp";
    server-fr-2-vpnbook = createConfig "fr-free-2.vpnbook.udp";
    server-fr-3-vpnbook = createConfig "fr-free-3.vpnbook.tcp";
    server-fr-4-vpnbook = createConfig "fr-free-4.vpnbook.udp";
  };

  security = {
    sudo = {
      enable = true;
    };
    # doas = {
    #   enable = true;
    #   # extraConfig = ''
    #   #   permit persist :wheel as root
    #   # '';
    #   extraRules = [
    #     # Allow execution of any command by any user in group wheel, requiring
    #     # a password and keeping any previously-defined environment variables.
    #     {
    #       groups = [ "wheel" ];
    #       noPass = false;
    #       keepEnv = true;
    #       persist = true;
    #     }
    #   ];
    # };
  };

  boot.loader.systemd-boot = { enable = true; };
  boot.loader.timeout = 35996;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=3s
  '';

  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 1d";
  };

  networking.hostName = sysname;
  networking.networkmanager.enable = true;
  
  console.keyMap = "us";

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = false;
  };
  users.defaultUserShell = pkgs.zsh;

  documentation.dev.enable = true;

  hardware.enableAllFirmware = true;

  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    jack.enable = true;
    audio.enable = true;
    pulse.enable = true;
  };
  # nixpkgs.config = {
  #   pulseaudio = true;
  #   allowUnfree = true;
  # };

  virtualisation.libvirtd.enable = true;
  programs = { virt-manager.enable = true; };

  system.stateVersion = "25.05";
}
