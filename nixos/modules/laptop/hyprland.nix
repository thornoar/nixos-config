{ pkgs, lib, readPackages, ... }:
{
    specialisation.hyprland.configuration = {
        boot.loader.systemd-boot.sortKey = "aaa";
        boot = {
            kernelParams = [ "nvidia-drm.fbdev=1" ];
            initrd.kernelModules = [ "nvidia" "i915" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
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
    };
}
