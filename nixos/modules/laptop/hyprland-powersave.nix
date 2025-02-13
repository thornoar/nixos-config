{ pkgs, lib, readPackages, ... }:
{
    specialisation.hyprland.configuration = {
        boot.loader.systemd-boot.sortKey = "aaa";
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
        environment.systemPackages = readPackages ../../src/packages/hyprland.txt pkgs;

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
        boot.blacklistedKernelModules = lib.mkDefault [ "nouveau" "nvidia" "nvidia_drm" "nvidia_modeset" ];

        hardware.nvidiaOptimus.disable = true;
    };
}
