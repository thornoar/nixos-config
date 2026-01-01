{ pkgs, ... }:

{
  environment.variables = {
    PCTYPE = "laptop";
    MUTTER_DEBUG_KMS_THREAD_TYPE = "user";
    # SPECIALISATION = lib.mkForce "hyprland";
    # SPECIALISATION_ENABLE = "0";
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
    light
    brightnessctl
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

    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
    '';

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
          # rightalt = "leftmeta";
          capslock = "esc";
          esc = "capslock";
        };
      };
    };

    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      # ly.enable = true;
      autoLogin = {
        enable = true;
        user = "ramak";
      };
    };

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      jack.enable = true;
      audio.enable = true;
      pulse.enable = true;
    };

    ollama = {
      package = pkgs.unstable.ollama;
      enable = true;
      acceleration = "cuda";
    };

    openvpn.servers = let
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

  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;

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
    # nerd-fonts.jetbrains-mono
    # dejavu_fonts
  ];

  programs.adb.enable = true;

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
}
