{ pkgs, ... }:
{
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
    waybar
  ]);
}
