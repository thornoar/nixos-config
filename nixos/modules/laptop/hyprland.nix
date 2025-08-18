{ pkgs, pkgs-unstable, lib, readPackages, config, ... }: {
  specialisation.hyprland.configuration = {
    boot.loader.systemd-boot.sortKey = "aaa";
    boot = {
      # kernelPackages = pkgs.linuxPackages_hardened;
      kernelParams = [ "nvidia-drm.fbdev=1" ];
      initrd.kernelModules =
        [ "nvidia" "i915" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
    };
    services.xserver.videoDrivers = [ "nvidia" ];
    programs.hyprland = {
      enable = true;
      package = pkgs.hyprland;
      xwayland.enable = true;
    };
    services = {
      picom = {
        enable = true;
        backend = "glx";
      };
      displayManager = {
        sddm.enable = true;
        sddm.wayland.enable = true;
      };
      ollama = {
        package = pkgs-unstable.ollama;
        enable = true;
        acceleration = "cuda";
      };
    };
    environment.variables = {
      SPECIALISATION = lib.mkForce "hyprland";
      WLR_NO_HARDWARE_CURSORS = "1";
      CURSOR_INACTIVE_TIMEOUT = "1";
      NIXOS_OZONE_WL = "1";
      HYPRCURSOR_SIZE = "16";
      TERMINAL = "alacritty";
      XCURSOR_SIZE = "16";
      BROWSER = "firefox -P hyprland";
    };
    environment.systemPackages =
      with pkgs-unstable; [ waybar ] ++
      readPackages ../../src/packages/hyprland.txt pkgs;

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
        # offload = { enable = true; enableOffloadCmd = true; };
        sync.enable = true;
        intelBusId = "PCI:0:0:2";
        nvidiaBusId = "PCI:0:1:0";
      };
    };
  };
}
