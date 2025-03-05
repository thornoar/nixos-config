{ pkgs, pkgs-unstable, config, lib, readPackages, ... }: {
  specialisation.xmonad.configuration = {
    boot.loader.systemd-boot.sortKey = "aac";
    environment.variables = {
      SPECIALISATION = lib.mkForce "xmonad";
      TERMINAL = "kitty";
      XCURSOR_SIZE = "16";
      BROWSER = "firefox -P xmonad";
    };
    services.xserver = {
      dpi = 192;
      # videoDrivers = [ "nvidia" ];
      enable = true;
      xkb.layout = "us";
      xkb.variant = "";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        # haskellPackages = pkgs-unstable.haskellPackages;
      };
      displayManager = {
        sessionCommands = ''
          # nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
          xrandr --output eDP-1 --scale 1.0x1.0
          setxkbmap -layout us,ru,de
          xset -dpms
          setterm -blank 0 -powerdown 0
          xset r rate 200 30
          xset s off
          transmission-daemon
          setxkbmap -option grp:alt_caps_toggle us,ru
        '';
      };
    };
    services = {
      picom = {
        enable = true;
        backend = "glx";
      };
      unclutter-xfixes = {
        enable = true;
        timeout = 1;
      };
    };
    environment.systemPackages =
      readPackages ../../src/packages/xmonad.txt pkgs;

    nixpkgs.config.nvidia.acceptLicense = true;
    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
      powerManagement.finegrained = false;
      nvidiaSettings = true;
      forceFullCompositionPipeline = false;
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.production;
      prime = {
        offload = {
          enable = true;
          enableOffloadCmd = true;
        };
        # sync.enable = true; 
        intelBusId = "PCI:0:0:2";
        nvidiaBusId = "PCI:0:1:0";
      };
    };
  };
}
