{ config, pkgs, ... }:

{
  home.username = "ramak";
  home.homeDirectory = "/home/ramak";
  xdg.userDirs = {
    enable = true;
    download = "${config.home.homeDirectory}/dls";
    desktop = "${config.home.homeDirectory}/dsk";
    documents = "${config.home.homeDirectory}/docs";
    pictures = "${config.home.homeDirectory}/media/pictures";
  };

  home.pointerCursor = {
    x11.enable = true;
    gtk.enable = true;
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
  };

  nixpkgs.config.allowUnfree = true;

  programs = { home-manager = { enable = true; }; };

  # nixpkgs.config = { permittedInsecurePackages = [ "libxls-1.6.2" ]; };

  home.stateVersion = "25.05";
}
