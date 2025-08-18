{ pkgs, pkgs-unstable, lib, readPackages, ... }: {
  specialisation.hyprland-powersave.configuration = {
    boot.loader.systemd-boot.sortKey = "aab";
    programs.hyprland = {
      enable = true;
      package = pkgs.hyprland;
      xwayland.enable = true;
    };
    services = {
      xserver.videoDrivers = lib.mkForce ["nvidia" "modesetting"];
      picom = {
        enable = true;
        backend = "glx";
      };
      displayManager = {
        sddm.enable = true;
        sddm.wayland.enable = true;
      };
    };
    powerManagement.cpuFreqGovernor = "powersave";
    environment.variables = {
      SPECIALISATION = lib.mkForce "hyprland-powersave";
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

    boot.extraModprobeConfig = lib.mkDefault ''
      blacklist nouveau
      options nouveau modeset=0
    '';

    services.udev.extraRules = lib.mkDefault ''
      # Remove NVIDIA USB xHCI Host Controller devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{power/control}="auto", ATTR{remove}="1"

      # Remove NVIDIA USB Type-C UCSI devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{power/control}="auto", ATTR{remove}="1"

      # Remove NVIDIA Audio devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{power/control}="auto", ATTR{remove}="1"

      # Remove NVIDIA VGA/3D controller devices
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", ATTR{power/control}="auto", ATTR{remove}="1"
    '';
    boot.blacklistedKernelModules = lib.mkDefault [
      "nouveau"
      "nvidia"
      "nvidia_drm"
      "nvidia_uvm"
      "i915"
      "nvidia_modeset"
    ];

    hardware.graphics.extraPackages = with pkgs; [ intel-compute-runtime ];

    hardware.nvidiaOptimus.disable = true;
    hardware.nvidia.prime.sync.enable = false;
    hardware.nvidia.open = false;
    nixpkgs.config.nvidia.acceptLicense = true;
    systemd.services.nbfc_service.enable = lib.mkForce false;
  };
}
