{ config, pkgs, lib, ... }:

let
  ts = builtins.toString;
  bc = lib.strings.removePrefix "#";
  clr = config.colors;
in {
  home.pointerCursor.size = 16;

  home.packages = (with pkgs; [
    mpd
    playerctl
    imagemagick
    graphicsmagick
    ffmpeg
    transmission_3
    killall
    dict
    libqalculate
    discord
    whatsie
    obs-studio
    zathura
    libnotify
    tagainijisho
    xournalpp
    maestral
    networkmanager-openvpn
    sc-im
    mpv
    neofetch
    caligula
    gimp3
    pdftk
    adbfs-rootless
    alsa-utils
    pulseaudio
    htop
    acpi
    lm_sensors
    cl-wordle
    graphviz
    libxkbcommon
    broot
    foot
    nps
    pastel
    wf-recorder
    wev
    flac
    lrcget
    python312Packages.syncedlyrics
  ]) ++ (with pkgs.unstable; [
    swayimg
    rmpc
    yt-dlp
    telegram-desktop
  ]);

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

  # xdg-mime configuration
  xdg.configFile."mimeapps.list".force = true;
  xdg.mimeApps = rec {
    enable = true;
    associations.added = {
      "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
      "audio/mpeg" = [ "mpv.desktop" ];
      "audio/mp3" = [ "mpv.desktop" ];
      "video/vnd.avi" = [ "mpv.desktop" ];
      "image/vnd.djvu+multipage" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
      "image/svg+xml" = [ "swayimg.desktop" ];
      "image/jpeg" = [ "swayimg.desktop" ];
      "image/png" = [ "swayimg.desktop" ];
      "text/csv" = [ "sc-im.desktop" ];
      "text/html" = [ "firefox.desktop" ];
    };
    defaultApplications = associations.added;
  };

  services.mpris-proxy.enable = true;

  services.dunst = {
    enable = true;
    settings.global = {
      origin = "top-center";
      offset = "${builtins.toString (config.window.windowSpaceOuter + 10)}x${builtins.toString (config.window.windowSpaceOuter + 10)}";
      progress_bar = false;
      frame_width = 1;
      gap_size = 2;
      font = config.misc.systemFont + " 11";
      corner_radius = config.window.rounding;
      frame_color = clr.primary;
      foreground = clr.white3;
      background = clr.bg0;
      sticky_history = false;
      padding = config.window.windowSpaceOuter + 5;
      horizontal_padding = config.window.windowSpaceOuter + 5;
      width = "(0,1000)";
    };
    settings.urgency_normal = { timeout = 1; };
  };

  # Mpv configuration
  xdg.configFile."mpv/mpv.conf" = config.util.dotFileMut "mpv.conf";

  # Zathura configuration
  xdg.configFile."zathura/zathurarc".text = ''
    set window-title-basename "true"
    set selection-clipboard "clipboard"

    set notification-error-bg       "${clr.red0}" # Red
    set notification-error-fg       "${clr.white1}" # Foreground
    set notification-warning-bg     "${clr.orange0}" # Orange
    set notification-warning-fg     "${clr.bg2}" # Selection
    set notification-bg             "${clr.bg0}" # Background
    set notification-fg             "${clr.white1}" # Foreground
    set completion-bg               "${clr.bg0}" # Background
    set completion-fg               "${clr.blue2}" # Comment
    set completion-group-bg         "${clr.bg0}" # Background
    set completion-group-fg         "${clr.blue2}" # Comment
    set completion-highlight-bg     "${clr.bg2}" # Selection
    set completion-highlight-fg     "${clr.white1}" # Foreground
    set index-bg                    "${clr.bg0}" # Background
    set index-fg                    "${clr.white1}" # Foreground
    set index-active-bg             "${clr.bg2}" # Current Line
    set index-active-fg             "${clr.white1}" # Foreground
    set inputbar-bg                 "${clr.bg0}" # Background
    set inputbar-fg                 "${clr.white1}" # Foreground
    set statusbar-bg                "${clr.bg0}" # Background
    set statusbar-fg                "${clr.white1}" # Foreground
    set highlight-color             "${clr.orange0}" # Orange
    set highlight-active-color      "${clr.magenta0}" # Pink
    set default-bg                  "${clr.bg0}" # Background
    set default-fg                  "${clr.white1}" # Foreground
    set render-loading              true
    set render-loading-fg           "${clr.bg0}" # Background
    set render-loading-bg           "${clr.white1}" # Foreground

    set recolor-lightcolor          "${clr.bg0}" # Background
    set recolor-darkcolor           "${clr.white1}" # Foreground

    set adjust-open width
    set recolor true
    set guioptions none

    map <S-Up> feedkeys "zI"
    map <S-Down> feedkeys "zO"
    map <M-Up> feedkeys "<PageUp>"
    map <M-Down> feedkeys "<PageDown>"

    set zoom-max 50000
  '';

  # Swayimg configuration
  xdg.configFile."swayimg/config" = config.util.dotFileMut "swayimg/config";
  xdg.configFile."swayimg/shared".text = ''
    [viewer]
    window = ${clr.bg0}ff

    [font]
    name = ${config.misc.systemFont}
    size = ${builtins.toString config.window.fontsize}
    color = #00000000
    shadow = #00000000
    background = #00000000
  '';

  # Tofi configuration
  xdg.configFile."tofi/config".text = ''
    font = "${config.misc.systemFont}"
    font-size = ${ts config.window.fontsize}
    text-color = ${clr.primary}
    prompt-color =${clr.primary} 
    placeholder-color = ${clr.bg3}
    input-color =${clr.primary} 
    default-result-color =${clr.primary} 
    selection-color = ${clr.fg0}
    selection-background = ${clr.bg0}
    selection-match-color = ${clr.magenta0}
    width = ${
      ts
      (config.window.widthPixels / 2 - 2 * config.window.windowSpaceOuter)
    }
    height = 50%
    background-color =${clr.bg0} 
    outline-width = 0
    outline-color =${clr.bg0} 
    border-width = 1
    border-color =${clr.primary} 
    corner-radius = ${ts config.window.rounding}
    # padding-top = ${ts (config.window.windowSpaceOuter / 2)}
    # padding-bottom = ${ts (config.window.windowSpaceOuter / 2)}
    # padding-left = ${ts config.window.windowSpaceOuter}
    # padding-right = ${ts config.window.windowSpaceOuter}
    padding-top = 5
    padding-bottom = 0
    padding-left = 10
    padding-right = 10
    margin-top = ${ts config.window.windowSpaceOuter}
    margin-bottom = 0
    margin-left = ${ts config.window.windowSpaceOuter}
    margin-right = ${ts config.window.windowSpaceOuter}

  '' + builtins.readFile ../src/tofi.conf;

  # Foot configuration
  xdg.configFile."foot/foot.ini" = config.util.dotFileMut "foot/foot.ini";
  xdg.configFile."foot/share.ini".text = ''
    font=${config.misc.systemFont}:size=${ts config.window.fontsize}, Noto Color Emoji

    pad=${ts config.window.terminalPaddingX}x${ts config.window.terminalPaddingY}

    [colors]
    alpha=${ts config.window.terminalOpacity}
    alpha-mode=default # Can be `default`, `matching` or `all`
    background=${bc clr.bg0}
    foreground=${bc clr.primary}

    regular0=${bc clr.black}    # black
    regular1=${bc clr.red1}     # red
    regular2=${bc clr.green1}   # green
    regular3=${bc clr.yellow1}  # yellow
    regular4=${bc clr.blue1}    # blue
    regular5=${bc clr.magenta1} # magenta
    regular6=${bc clr.cyan}     # cyan
    regular7=${bc clr.white0}   # white

    bright0=${bc clr.black}       # bright black
    bright1=${bc clr.red1}        # bright red
    bright2=${bc clr.green1}      # bright green
    bright3=${bc clr.yellow1}     # bright yellow
    bright4=${bc clr.blue1}       # bright blue
    bright5=${bc clr.magenta1}    # bright magenta
    bright6=${bc clr.cyan}        # bright cyan
    bright7=${bc clr.white0}      # bright white

    # dim-blend-towards=black
    dim0=${bc clr.black}   
    dim1=${bc clr.red1}    
    dim2=${bc clr.green1}  
    dim3=${bc clr.yellow1} 
    dim4=${bc clr.blue1}   
    dim5=${bc clr.magenta1}
    dim6=${bc clr.cyan}    
    dim7=${bc clr.white0}  

    ## Sixel colors
    # sixel0 =  000000
    # sixel1 =  3333cc
    # sixel2 =  cc2121
    # sixel3 =  33cc33
    # sixel4 =  cc33cc
    # sixel5 =  33cccc
    # sixel6 =  cccc33
    # sixel7 =  878787
    # sixel8 =  424242
    # sixel9 =  545499
    # sixel10 = 994242
    # sixel11 = 549954
    # sixel12 = 995499
    # sixel13 = 549999
    # sixel14 = 999954
    # sixel15 = cccccc

    ## Misc colors
    # selection-foreground=<inverse foreground/background>
    # selection-background=<inverse foreground/background>
    # jump-labels=regular0 regular3               # black-on-yellow
    # scrollback-indicator=<regular0> <bright4>   # black-on-bright-blue
    # search-box-no-match=<regular0> <regular1>   # black-on-red
    # search-box-match=<regular0> <regular3>      # black-on-yellow
    # urls=<regular3>
  '';

  # Waybar configuration
  xdg.configFile."waybar/shared.css".text = ''
    * {
        font-size: ${ts config.window.fontsizeWaybar}pt;
        border-radius: ${ts config.window.rounding}px;
        font-family: "${config.misc.systemFont}";
    }
  '';
  xdg.configFile."waybar/style.css" = config.util.dotFileMut "waybar/style.css";
  xdg.configFile."waybar/colors.css".text = (config.util.toCSS false "" config.colors);
  xdg.configFile."waybar/config".text = ''
    {
        "layer": "top",
        "margin-top": ${ts config.window.windowSpaceOuter},
        "margin-bottom": 0,
        "margin-left": ${ts config.window.windowSpaceOuter},
        "margin-right": ${ts config.window.windowSpaceOuter},
        "layer": "top",
        "spacing": 0,
        "height": 24,

        "include": [
            "~/.config/waybar/modules.json"
        ],

        "modules-left": [
            "hyprland/workspaces",
        ],

        "modules-center": [
            "pulseaudio",
            "clock",
            "backlight",
        ],

        "modules-right": [
            "hyprland/language",
            "network",
            "cpu",
            "memory",
            "battery", 
        ]
    }
  '';
  xdg.configFile."waybar/modules.json" = config.util.dotFileMut "waybar/modules.json";

  # Wpaperd configuration
  xdg.configFile."wpaperd/config.toml".text = ''
    [${config.misc.monitorName}]
    path = "/home/ramak/media/wallpapers/${config.wallpaper.dir}"
    duration = "3m"
  '';

  # Swaylock configuration
  programs.swaylock.enable = true;
  xdg.configFile."swaylock/config".text = ''
    daemonize
    hide-keyboard-layout
    indicator-radius=115
    indicator-thickness=25
    line-color=${bc clr.bg0}00
    ring-color=${bc clr.bg0}00
    inside-color=${bc clr.bg0}00
    key-hl-color=${bc clr.primary}
    separator-color=${bc clr.bg0}00
    text-color=${bc clr.primary}
    text-caps-lock-color=${bc clr.primary}00
    line-ver-color=${bc clr.bg0}00
    ring-ver-color=${bc clr.green5}
    inside-ver-color=${bc clr.bg0}00
    text-ver-color=${bc clr.primary}00
    ring-wrong-color=${bc clr.red1}
    text-wrong-color=${bc clr.red1}00
    inside-wrong-color=${bc clr.red1}00
    inside-clear-color=${bc clr.bg0}00
    text-clear-color=${bc clr.primary}00
    ring-clear-color=${bc clr.yellow1}
    line-clear-color=${bc clr.bg0}00
    line-wrong-color=${bc clr.bg0}00
    bs-hl-color=${bc clr.yellow1}
    ignore-empty-password
    indicator-idle-visible
    image=~/.config/swaylock/background.png
  '';
  xdg.configFile."swaylock/background.png" = config.util.dotFileMut "swaylock/background.png";
}
