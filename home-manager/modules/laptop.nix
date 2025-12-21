{ config, pkgs, lib, ... }:

{
  home.pointerCursor.size = 16;

  home.sessionVariables = {
    WALLPAPER_DIR = config.wallpaper.dir;
  };

  home.packages =
    pkgs.tools.readPackages ../src/packages/general.txt pkgs
    ++ pkgs.tools.readPackages ../src/packages/insecure.txt pkgs
    ++ pkgs.tools.readPackages ../src/packages/unstable.txt pkgs.unstable;

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };

  gtk = {
    enable = true;
    font.name = config.misc.systemFont + " 11";
    theme = {
      name = "deepin-dark";
      package = pkgs.deepin.deepin-gtk-theme;
    };
  };

  # wayland.windowManager.hyprland = {
  #   enable = true;
  #   plugins = with pkgs.hyprlandPlugins; [
  #     hyprexpo
  #   ];
  # };

  # services.keynav.enable = true;

  services.mpris-proxy.enable = true;

  services.dunst = {
    enable = true;
    settings.global = {
      origin = "top";
      offset = "${builtins.toString (config.hyprland.windowSpaceOuter + 10)}x${builtins.toString (config.hyprland.windowSpaceOuter + 10)}";
      progress_bar = false;
      frame_width = 1;
      gap_size = 2;
      font = config.misc.systemFont + " 11";
      corner_radius = config.hyprland.rounding;
      frame_color = config.colors.primary;
      foreground = config.colors.white3;
      background = config.colors.bg0;
      sticky_history = false;
      padding = config.hyprland.windowSpaceOuter + 5;
      horizontal_padding = config.hyprland.windowSpaceOuter + 5;
    };
    settings.urgency_normal = { timeout = 1; };
  };

  xdg.configFile."colors.css".text = (config.util.toCSS false "" config.colors);

  # Moc configuration
  home.file.".moc/config" = config.util.dotFileMut "mocp/config";
  home.file.".moc/keymap" = config.util.dotFileMut "mocp/keymap";
  home.file.".moc/themes/ramak" = config.util.dotFileMut "mocp/theme";

  # MPD configuration
  xdg.configFile."mpd/mpd.conf" = config.util.dotFileMut "mpd/mpd.conf";

  # Mpv configuration
  xdg.configFile."mpv/mpv.conf" = config.util.dotFileMut "mpv.conf";

  # Zathura configuration
  xdg.configFile."zathura/zathurarc".text = ''
    set window-title-basename "true"
    set selection-clipboard "clipboard"

    set notification-error-bg       "${config.colors.red0}" # Red
    set notification-error-fg       "${config.colors.white1}" # Foreground
    set notification-warning-bg     "${config.colors.orange0}" # Orange
    set notification-warning-fg     "${config.colors.bg2}" # Selection
    set notification-bg             "${config.colors.bg0}" # Background
    set notification-fg             "${config.colors.white1}" # Foreground
    set completion-bg               "${config.colors.bg0}" # Background
    set completion-fg               "${config.colors.blue2}" # Comment
    set completion-group-bg         "${config.colors.bg0}" # Background
    set completion-group-fg         "${config.colors.blue2}" # Comment
    set completion-highlight-bg     "${config.colors.bg2}" # Selection
    set completion-highlight-fg     "${config.colors.white1}" # Foreground
    set index-bg                    "${config.colors.bg0}" # Background
    set index-fg                    "${config.colors.white1}" # Foreground
    set index-active-bg             "${config.colors.bg2}" # Current Line
    set index-active-fg             "${config.colors.white1}" # Foreground
    set inputbar-bg                 "${config.colors.bg0}" # Background
    set inputbar-fg                 "${config.colors.white1}" # Foreground
    set statusbar-bg                "${config.colors.bg0}" # Background
    set statusbar-fg                "${config.colors.white1}" # Foreground
    set highlight-color             "${config.colors.orange0}" # Orange
    set highlight-active-color      "${config.colors.magenta0}" # Pink
    set default-bg                  "${config.colors.bg0}" # Background
    set default-fg                  "${config.colors.white1}" # Foreground
    set render-loading              true
    set render-loading-fg           "${config.colors.bg0}" # Background
    set render-loading-bg           "${config.colors.white1}" # Foreground

    set recolor-lightcolor          "${config.colors.bg0}" # Background
    set recolor-darkcolor           "${config.colors.white1}" # Foreground

    set adjust-open width
    set recolor false
    set guioptions none

    map <S-Up> feedkeys "zI"
    map <S-Down> feedkeys "zO"
    map <M-Up> feedkeys "<PageUp>"
    map <M-Down> feedkeys "<PageDown>"

    set zoom-max 50000
  '';
}
