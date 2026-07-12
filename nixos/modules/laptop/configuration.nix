{ pkgs, lib, ... }:

{
  environment.variables = {
    PCTYPE = "laptop";
    MUTTER_DEBUG_KMS_THREAD_TYPE = "user";
    WLR_NO_HARDWARE_CURSORS = "1";
    CURSOR_INACTIVE_TIMEOUT = "1";
    NIXOS_OZONE_WL = "1";
    HYPRCURSOR_SIZE = "16";
    TERMINAL = "foot";
    XCURSOR_SIZE = "16";
    BROWSER = "firefox";
    XDG_SESSION_TYPE = "wayland";
    GBM_BACKEND = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    LIBVA_DRIVER_NAME = "nvidia";
  };

  environment.systemPackages = with pkgs; [
    # light
    brightnessctl
    spice-gtk
  ];

  services = {
    libinput = {
      enable = true;
      touchpad = {
        naturalScrolling = true;
        tapping = false;
        clickMethod = "clickfinger";
        disableWhileTyping = true;
      };
    };

    logind.settings.Login = {
      HandleLidSwitch = "ignore";
      HandleLidSwitchExternalPower = "ignore";
      KillUserProcesses = true;
    };

    keyd = {
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
          kpenter = "leftmeta";
          kp5 = "down";
          kpdelete = "delete";
          capslock = "esc";
          esc = "capslock";
        };
      };
    };

    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        waylandFrontend = true;
        addons = with pkgs; [
          fcitx5-mozc
          fcitx5-gtk
        ];
      };
    };

    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      # ly.enable = true;
      defaultSession = "niri";
      autoLogin = {
        enable = true;
        user = "ramak";
      };
    };

    # pulseaudio = {
    #   enable = true;
    #   support32Bit = true;
    # };

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      # systemWide = true;
      jack.enable = true;
      audio.enable = true;
      pulse.enable = true;
    };

    ollama = {
      package = pkgs.unstable.ollama-cuda;
      enable = true;
      # acceleration = "cuda";
    };

    openvpn.servers = let
      createConfig = name: {
        config = "config /home/ramak/projects/nixos-config/nixos/src/openvpn/${name}.ovpn";
        # config = "config ${fname}";
        updateResolvConf = true;
        autoStart = false;
      };
      fname2attrs = name: { name = "server-" + name; value = createConfig name; };
    in builtins.listToAttrs 
      (lib.lists.forEach [
          "us-1-protonvpn"
          "nl-1-protonvpn"
          "jp-1-protonvpn"
        ] fname2attrs);

    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
        hplip
        splix
        foomatic-db-ppds
        foomatic-db-nonfree
        # (writeTextDir "share/cups/model/xerox/wc3025.ppd" (builtins.readFile ../../src/Xerox_WorkCentre_3025.ppd))
      ];
    };

    # avahi = {
    #   enable = true;
    #   nssmdns4 = true;
    #   openFirewall = true;
    # };
  };

  # hardware.sane = {
  #   enable = true;
  #   netConf = "192.168.0.34";
  #   openFirewall = true;
  # };

  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
  programs.virt-manager.enable = true;
  # security.wrappers.spice-client-glib-usb-acl-helper = {
  #   source = "${pkgs.spice-gtk}/bin/spice-client-glib-usb-acl-helper";
  #   owner = "nixos";
  # };

  # nixpkgs.config = {
  #   pulseaudio = true;
  #   allowUnfree = true;
  # };

  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    };
    gamemode.enable = true;
  };

  time.timeZone = "Europe/Belgrade";

  fonts.packages = with pkgs; [
    # noto-fonts
    ipafont
    kochi-substitute
    newcomputermodern
    hack-font
    jetbrains-mono
    # nerd-fonts.hack
    nerd-fonts.jetbrains-mono
    # dejavu_fonts
  ] ++ [ (pkgs.callPackage ./font-handwriting.nix { inherit pkgs; }) ];

  # programs.adb.enable = true;

  security.pam.services.swaylock = {};

  systemd.timers."refresh-nps-cache" = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
        Persistent = true;
      Unit = "refresh-nps-cache.service";
    };
  };
  systemd.services."refresh-nps-cache" = {
    path = [ "/run/current-system/sw/" ];
    serviceConfig = {
      Type = "oneshot";
      User = "ramak";
    };
    script = ''
      set -eu
      ${pkgs.nps}/bin/nps -e -r -dddd
    '';
  };

  systemd.settings.Manager = {
    DefaultTimeoutStopSec = "5s";
    RebootWatchdogSec = "1min";
  };
  systemd.user.extraConfig = ''
    DefaultTimeoutStopSec=5s
  '';

  # environment.variables = {
  #   "QT_STYLE_OVERRIDE" = pkgs.lib.mkForce "adwaita-dark";
  # };


  systemd.services."auto" = {
    enable = true;
    wantedBy = ["multi-user.target"];
    after = ["graphical-session.target"];
    before = ["umount.target" "shutdown.target" "reboot.target" "halt.target"];
    unitConfig = {
      RequiresMountsFor = ["/home/ramak/media" "/home/ramak/media/music" "/home/ramak/media/films"];
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = "/bin/sh /home/ramak/.local/bin/onboot";
      # ExecStart = "${pkgs.coreutils}/bin/true";
      ExecStop = "/bin/sh /home/ramak/.local/bin/onshutdown";
      RemainAfterExit = true;
      User = "ramak";
    };
  };
}
