{ pkgs, lib, ... }:
{
  boot.loader.systemd-boot.sortKey = "aaa";

  programs.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    xwayland.enable = true;
  };

  services = {
    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      # ly.enable = true;
      autoLogin = {
        enable = true;
        user = "ramak";
      };
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
}
