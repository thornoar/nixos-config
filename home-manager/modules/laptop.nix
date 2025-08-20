{ config, pkgs, lib, readPackages, ... }:

let
  ts = builtins.toString;
  toCSS = mkstr:
    lib.attrsets.foldlAttrs (str: k: v:
      str + ''
        @define-color ${k} ${if mkstr then ''"'' + ts v + ''"'' else ts v};
      '');
in 
{
  config = {
    home.pointerCursor.size = 16;

    home.sessionVariables = {
      WALLPAPER_DIR = config.wallpaper.dir;
      ARCHVM = "192.168.122.35";
      DEBIANVM = "192.168.122.127";
    };

    home.packages =
      readPackages ../src/packages/general.txt pkgs
      ++ readPackages ../src/packages/insecure.txt pkgs;

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

    xdg.configFile."colors.css".text = (toCSS false "" config.colors);

    # moc configuration
    home.file.".moc/config".text = ''
      Theme = nightly_theme
      Keymap = keymap
      Repeat = yes
      Shuffle = no
      AutoNext = yes
    '';
    home.file.".moc/keymap".text = ''
      go    = ENTER RIGHT
      go_up = U LEFT
      #seek_forward  = RIGHT
      #seek_backward = LEFT
    '';

    # mpv configuration
    xdg.configFile."mpv/mpv.conf".text = ''
      ao=alsa
      audio-device=auto
      loop-file=inf
      sub-file-paths=RusSubs:subs:subtitles
    '';

    # zathura configuration
    xdg.configFile."zathura/zathurarc".text = ''
      set window-title-basename "true"
      set selection-clipboard "clipboard"

      set notification-error-bg       "${config.colors.colorRed0}" # Red
      set notification-error-fg       "${config.colors.colorWhite1}" # Foreground
      set notification-warning-bg     "${config.colors.colorOrange0}" # Orange
      set notification-warning-fg     "${config.colors.bgColor2}" # Selection
      set notification-bg             "${config.colors.bgColor0}" # Background
      set notification-fg             "${config.colors.colorWhite1}" # Foreground
      set completion-bg               "${config.colors.bgColor0}" # Background
      set completion-fg               "${config.colors.colorBlue2}" # Comment
      set completion-group-bg         "${config.colors.bgColor0}" # Background
      set completion-group-fg         "${config.colors.colorBlue2}" # Comment
      set completion-highlight-bg     "${config.colors.bgColor2}" # Selection
      set completion-highlight-fg     "${config.colors.colorWhite1}" # Foreground
      set index-bg                    "${config.colors.bgColor0}" # Background
      set index-fg                    "${config.colors.colorWhite1}" # Foreground
      set index-active-bg             "${config.colors.bgColor2}" # Current Line
      set index-active-fg             "${config.colors.colorWhite1}" # Foreground
      set inputbar-bg                 "${config.colors.bgColor0}" # Background
      set inputbar-fg                 "${config.colors.colorWhite1}" # Foreground
      set statusbar-bg                "${config.colors.bgColor0}" # Background
      set statusbar-fg                "${config.colors.colorWhite1}" # Foreground
      set highlight-color             "${config.colors.colorOrange0}" # Orange
      set highlight-active-color      "${config.colors.colorMagenta0}" # Pink
      set default-bg                  "${config.colors.bgColor0}" # Background
      set default-fg                  "${config.colors.colorWhite1}" # Foreground
      set render-loading              true
      set render-loading-fg           "${config.colors.bgColor0}" # Background
      set render-loading-bg           "${config.colors.colorWhite1}" # Foreground

      set recolor-lightcolor          "${config.colors.bgColor0}" # Background
      set recolor-darkcolor           "${config.colors.colorWhite1}" # Foreground

      set adjust-open width
      # set recolor true
      set guioptions none

      map <S-Up> feedkeys "zI"
      map <S-Down> feedkeys "zO"
      map <M-Up> feedkeys "<PageUp>"
      map <M-Down> feedkeys "<PageDown>"

      set zoom-max 50000
    '';
  };
}
