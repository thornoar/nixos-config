{ config, pkgs, inputs, system, lib, pkgs-unstable, readFile, readPackages, ... }:

let
  readCustomPackages = file:
    lib.lists.forEach (readFile file)
    (x: inputs.${x}.packages.${system}.default);
  ts = builtins.toString;
in {
  home.sessionVariables = {
    WALLPAPER_DIR = config.wallpaper.dir;
  };

  xdg.configFile."mimeapps.list".force = true;
  xdg.mimeApps = rec {
    enable = true;
    associations.added = {
      "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
      "audio/mpeg" = [ "mpv.desktop" ];
      "audio/mp3" = [ "mpv.desktop" ];
      "video/vnd.avi" = [ "mpv.desktop" ];
      "image/vnd.djvu+multipage" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
      "image/svg+xml" = [ "imv.desktop" ];
      "text/csv" = [ "sc-im.desktop" ];
      "text/html" = [ "firefox.desktop" ];
    };
    defaultApplications = associations.added;
  };

  home.packages = with pkgs;
    [
      (python3.withPackages
        (ps: with ps; [ manim ipython sympy numpy ollama openai ]))
      (rWrapper.override {
        packages = with rPackages; [
          languageserver
          ggplot2
          dplyr
          xts
          pracma
          latex2exp
        ];
      })
      (haskellPackages.ghcWithPackages (hspkgs: with hspkgs; [
        QuickCheck
      ]))
      (texlive.combine { inherit (texlive) scheme-full; })
    ] ++ readPackages ../src/packages/development.txt pkgs
    ++ readPackages ../src/packages/general.txt pkgs
    ++ readPackages ../src/packages/unstable.txt pkgs-unstable
    ++ readPackages ../src/packages/insecure.txt pkgs
    ++ readCustomPackages ../src/packages/custom.txt;

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };

  gtk = {
    enable = true;
    font.name = "Hack Mono 11";
    theme = {
      name = "deepin-dark";
      package = pkgs.deepin.deepin-gtk-theme;
    };
  };

  services.keynav.enable = true;

  services.mpris-proxy.enable = true;

  services.dunst = {
    enable = true;
    settings.global = {
      origin = "top-right";
      offset = "${ts (config.hyprland.windowSpaceOuter + 10)}x${ts (config.hyprland.windowSpaceOuter + 10)}";
      progress_bar = false;
      frame_width = 1;
      gap_size = 2;
      font = "Hack Nerd Font Mono 11";
      corner_radius = config.hyprland.rounding;
      frame_color = config.colors.colorMagenta0;
      foreground = config.colors.colorWhite3;
      background = config.colors.bgColor0;
      sticky_history = false;
      padding = config.hyprland.windowSpaceOuter;
      horizontal_padding = config.hyprland.windowSpaceOuter;
    };
    settings.urgency_normal = { timeout = 1; };
  };
}
