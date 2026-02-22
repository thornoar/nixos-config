{ sysname, pkgs, config, lib, ... }:

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
      "kvm"
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
      keep-outputs = true
      keep-derivations = true
    '';
    registry = {
      nixpkgs.flake = pkgs.inputs.nixpkgs;
      nixpkgs-unstable.flake = pkgs.inputs.nixpkgs-unstable;
      ${sysname}.flake = pkgs.inputs.self;
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
      # NIXPKGS_ALLOW_BROKEN = "1";
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
      EDITOR = "vim";
      VISUAL = EDITOR;
      HISTCONTROL = "ignoreboth";
      READER = "zathura";
      FILEMANAGER = "br";
      IPYTHONDIR = "${XDG_CONFIG_HOME}/ipython";
      CARGO_HOME = "${XDG_DATA_HOME}/cargo";
      LESSHISTFILE = "${XDG_CACHE_HOME}/less/history";
      CUDA_CACHE_PATH = "${XDG_CACHE_HOME}/nv";
      XCOMPOSECACHE = "${XDG_CACHE_HOME}/X11/xcompose";
      ASYMPTOTE_PDFVIEWER = "$HOME/.nix-profile/bin/zathura";
      # SPECIALISATION = "default";
      NIX_PACKAGE_SEARCH_EXPERIMENTAL = "true";
      NIX_PACKAGE_SEARCH_EXACT_COLOR = "cyan";
      NIX_PACKAGE_SEARCH_DIRECT_COLOR = "yellow";
      NIX_PACKAGE_SEARCH_INDIRECT_COLOR = "white";
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
      update-resolv-conf
      which
      file
      powertop
      gparted
      openssl
      lsof
      tree
    ];
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
  # programs.ssh.startAgent = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-tty;
  };

  security = {
    sudo = {
      enable = true;
    };
  };

  nix.gc = {
    automatic = false;
    # dates = "daily";
    # options = "--delete-older-than 1d";
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

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
    nixos = {
      enable = true;
      includeAllModules = true;
    };
  };

  hardware.enableAllFirmware = true;

  services.fwupd = {
    enable = true;
  };

  services.locate.enable = true;

  environment.etc."current-system-packages".text =
    let packages = builtins.map (p: "${p.name}") config.environment.systemPackages;
        sortedUnique = builtins.sort builtins.lessThan (lib.unique packages);
     in builtins.concatStringsSep "\n" sortedUnique;

  system.stateVersion = "25.05";
}
