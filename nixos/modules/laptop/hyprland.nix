{ pkgs, lib, ... }:
{
  boot.loader.systemd-boot.sortKey = "aaa";

  programs.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    xwayland.enable = true;
  };

  services = {
    xserver.videoDrivers = lib.mkForce ["nvidia" "modesetting"];
    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      # ly.enable = true;
      autoLogin = {
        enable = true;
        user = "ramak";
      };
    };
    ollama = {
      package = pkgs.unstable.ollama;
      enable = true;
      acceleration = "cuda";
    };
  };

  environment.variables = {
    SPECIALISATION = lib.mkForce "hyprland-powersave";
    SPECIALISATION_ENABLE = "0";
    WLR_NO_HARDWARE_CURSORS = "1";
    CURSOR_INACTIVE_TIMEOUT = "1";
    NIXOS_OZONE_WL = "1";
    HYPRCURSOR_SIZE = "16";
    TERMINAL = "alacritty";
    XCURSOR_SIZE = "16";
    BROWSER = "firefox -P hyprland";
  };

  environment.systemPackages =
    with pkgs.unstable; [ waybar ] ++
    pkgs.tools.readPackages ../../src/packages/hyprland.txt pkgs;

  boot.extraModprobeConfig = lib.mkDefault ''
    blacklist nouveau
    options nouveau modeset=0
  '';

  hardware.graphics.extraPackages = with pkgs; [ intel-compute-runtime ];

  hardware.nvidiaOptimus.disable = true;
  hardware.nvidia = {
    # modesetting.enable = true;
    prime.sync.enable = false;
    open = false;
    # prime = {
    #   offload = {
    #     enable = true;
    #     enableOffloadCmd = true;
    #   };
    #   intelBusId = "PCI:0:2:0";
    #   nvidiaBusId = "PCI:1:0:0";
    # };
  };

  systemd.services.nbfc_service.enable = lib.mkForce false;
}
