{ pkgs, ... }:
{
  programs.niri = {
    enable = true;
    package = pkgs.unstable.niri;
    # package = pkgs.inputs.niri.packages.${pkgs.system}.default;
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
