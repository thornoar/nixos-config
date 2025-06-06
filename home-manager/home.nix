{ config, pkgs, inputs, system, lib, pkgs-unstable, readFile, readPackages, ... }:

let
  readCustomPackages = file:
    lib.lists.forEach (readFile file)
    (x: inputs.${x}.packages.${system}.default);
  ts = builtins.toString;
in {
  home.username = "ramak";
  home.homeDirectory = "/home/ramak";
  xdg.userDirs = {
    enable = true;
    download = "${config.home.homeDirectory}/dls";
    desktop = "${config.home.homeDirectory}/dsk";
    documents = "${config.home.homeDirectory}/docs";
    pictures = "${config.home.homeDirectory}/media/pictures";
  };
  home.sessionVariables = {
    BAT_THEME = "base16";
  };

  nixpkgs.config.allowUnfree = true;

  programs = { home-manager = { enable = true; }; };

  nixpkgs.config = { permittedInsecurePackages = [ "libxls-1.6.2" ]; };

  home.stateVersion = "23.11";
}
