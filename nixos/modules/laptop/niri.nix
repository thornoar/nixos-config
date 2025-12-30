{ pkgs, ... }:
{
  programs.niri = {
    enable = true;
    package = pkgs.unstable.niri;
  };

  environment.systemPackages = (with pkgs; [
    wpaperd
    tofi
    wl-clipboard
    warpd
    waybar
    xwayland-satellite
  ]);
}
