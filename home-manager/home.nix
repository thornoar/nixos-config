{ config, pkgs, inputs, system, lib, pkgs-unstable, readFile, readPackages, ...
}:

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
    WALLPAPER_DIR = config.wallpaper.dir;
  };

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
    };
    defaultApplications = associations.added;
  };
  nixpkgs.config.allowUnfree = true;

  home.packages = readPackages ./src/packages/general.txt pkgs
    ++ readPackages ./src/packages/unstable.txt pkgs-unstable
    ++ readPackages ./src/packages/insecure.txt pkgs
    ++ readCustomPackages ./src/packages/custom.txt;

  programs = { home-manager = { enable = true; }; };

  home.pointerCursor = {
    x11.enable = true;
    gtk.enable = true;
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
  };

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
      offset = "${ts (config.hyprland.windowSpaceOuter + 4)}x${
          ts (config.hyprland.windowSpaceOuter + 4)
        }";
      progress_bar = false;
      frame_width = 1;
      gap_size = 2;
      font = "Hack Nerd Font Mono 11";
      corner_radius = config.hyprland.rounding;
      frame_color = config.colors.colorMagenta1;
      foreground = config.colors.colorWhite3;
      background = config.colors.bgColor1;
      sticky_history = false;
      padding = config.hyprland.windowSpaceOuter;
      horizontal_padding = config.hyprland.windowSpaceOuter;
    };
    settings.urgency_normal = { timeout = 1; };
  };

  nixpkgs.config = { permittedInsecurePackages = [ "libxls-1.6.2" ]; };

  home.stateVersion = "23.11";
}
