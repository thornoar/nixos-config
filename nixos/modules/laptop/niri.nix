{ pkgs, lib, ... }:
{
  programs.niri = {
    enable = true;
    package = pkgs.unstable.niri;
  };

  # environment.variables = {
  #   SPECIALISATION = lib.mkForce "niri";
  #   SPECIALISATION_ENABLE = "0";
  #   WLR_NO_HARDWARE_CURSORS = "1";
  #   CURSOR_INACTIVE_TIMEOUT = "1";
  #   NIXOS_OZONE_WL = "1";
  #   # HYPRCURSOR_SIZE = "16";
  #   TERMINAL = "foot";
  #   XCURSOR_SIZE = "16";
  #   BROWSER = "firefox";
  # };
}
