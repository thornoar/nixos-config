{ pkgs, lib, ... }:
{
  boot.loader.systemd-boot.sortKey = "aaa";

  programs.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    withUWSM = true;
    # xwayland.enable = true;
  };
  environment.sessionVariables.HYPR_PLUGIN_DIR = pkgs.symlinkJoin {
    name = "hyprland-plugins";
    paths = with pkgs.hyprlandPlugins; [
      hyprexpo
    ];
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

  environment.systemPackages = (with pkgs; [   
    wpaperd
    tofi
    wl-clipboard
    xsel
    warpd
    hyprpicker
    hyprshot
    hyprprop
    glib
    waydroid
  ]) ++ (with pkgs.unstable; [
    waybar
  ]);
}
