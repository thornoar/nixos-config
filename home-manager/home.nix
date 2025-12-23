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

  home.sessionVariables = {
    WALLPAPER_DIR = config.wallpaper.dir;
  };

  nixpkgs.config.allowUnfree = true;

  programs = { home-manager = { enable = true; }; };

  # zsh configuration
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = false;
    syntaxHighlighting.enable = true;
    shellAliases = {};
    initContent = "source ~/.config/zsh/initExtra.zsh";
    envExtra = "source ~/.config/zsh/envExtra.zsh";
  };
  xdg.configFile."zsh/initExtra.zsh" = config.util.dotFileMut "zsh/initExtra.zsh";
  xdg.configFile."zsh/envExtra.zsh" = config.util.dotFileMut "zsh/envExtra.zsh";
  xdg.configFile."nix-develop/.zshrc" = config.util.dotFileMut "zsh/nix-develop.zsh";
  xdg.configFile."special-terminal/.zshrc" = config.util.dotFileMut "zsh/special-terminal.zsh";
  xdg.configFile."special-music/.zshrc" = config.util.dotFileMut "zsh/special-music.zsh";
  xdg.configFile."nix-develop/.zshenv" = config.util.dotFileMut "zsh/envExtra.zsh";
  xdg.configFile."special-terminal/.zshenv" = config.util.dotFileMut "zsh/envExtra.zsh";
  xdg.configFile."special-music/.zshenv" = config.util.dotFileMut "zsh/envExtra.zsh";

  # Neofetch configuration
  xdg.configFile."neofetch/config.conf" = config.util.dotFileMut "neofetch.conf";

  # Khal configuration
  xdg.configFile."khal/config" = config.util.dotFileMut "khal.config.ini";

  # Tmux configuration
  xdg.configFile."tmux/tmux.conf" = config.util.dotFileMut "tmux/tmux.conf";

  # Htop configuration
  xdg.configFile."htop/htoprc" = config.util.dotFileMut "htop/htoprc";

  # Pshash config
  xdg.configFile."pshash/pshash.conf" = config.util.dotFileMut "pshash.conf";

  # Cheat config
  xdg.configFile."cheat/conf.yml" = config.util.dotFileMut "cheat.conf.yml";

  # Broot configuration
  xdg.configFile."broot/conf.hjson" = config.util.dotFileMut "broot/conf.hjson";
  xdg.configFile."broot/verbs.hjson" = config.util.dotFileMut "broot/verbs.hjson";
  xdg.configFile."broot/colorscheme.hjson".text = ''
    skin: {
        default: "${config.colors.white3} None / ${config.colors.white4} None"
        tree: "${config.colors.bg3} None / ${config.colors.bg2} None"
        parent: "${config.colors.green1} None / ${config.colors.green4} None"
        file: "${config.colors.green5} None / ${config.colors.green5} None"
        directory: "${config.colors.primary} None Bold / ${config.colors.primary} None"
        exe: "${config.colors.green1} None"
        link: "${config.colors.cyan} None"
        pruning: "${config.colors.white4} None"
        perm__: "None None"
        perm_r: "${config.colors.green2} None"
        perm_w: "${config.colors.green5} None"
        perm_x: "${config.colors.green1} None"
        owner: "${config.colors.primary} None"
        group: "${config.colors.white3} None"
        count: "${config.colors.white4} None"
        size: "${config.colors.white4} None"
        dates: "${config.colors.white3} None"
        sparse: "${config.colors.yellow0} None"
        content_extract: "ansi(29) None"
        content_match: "${config.colors.red0} None Bold"
        git_branch: "${config.colors.white2} None"
        git_insertions: "${config.colors.green4} None"
        git_deletions: "${config.colors.red1} None"
        git_status_current: "${config.colors.bg2} None"
        git_status_modified: "${config.colors.blue1} None"
        git_status_new: "${config.colors.green4} None"
        git_status_ignored: "${config.colors.white3} None"
        git_status_conflicted: "${config.colors.red1} None"
        git_status_other: "${config.colors.red1} None"
        selected_line: "None ${config.colors.bg2} / None ${config.colors.bg2}"
        char_match: "${config.colors.red0} None"
        file_error: "${config.colors.red1} None"
        flag_label: "${config.colors.white4} None"
        flag_value: "${config.colors.green5} None"
        input: "${config.colors.white2} None / ${config.colors.white4} None"
        status_error: "${config.colors.white3} ${config.colors.red0}"
        status_job: "${config.colors.yellow1} ${config.colors.bg1}"
        status_normal: "None ${config.colors.bg0} / None None"
        status_italic: "${config.colors.green5} ${config.colors.bg0} / None None"
        status_bold: "${config.colors.green5} ${config.colors.bg0} / None None"
        status_code: "${config.colors.white2} ${config.colors.bg0} / None None"
        status_ellipsis: "${config.colors.white2} ${config.colors.bg0} / None None"
        purpose_normal: "None None"
        purpose_italic: "${config.colors.green5} None"
        purpose_bold: "${config.colors.green5} None"
        purpose_ellipsis: "None None"
        scrollbar_track: "${config.colors.bg0} None"
        scrollbar_thumb: "${config.colors.white3} None / ${config.colors.white4} None"
        help_paragraph: "None None"
        help_bold: "${config.colors.green5} None"
        help_italic: "${config.colors.cyan} None"
        help_code: "${config.colors.green4} ${config.colors.bg1}"
        help_headers: "${config.colors.green5} None Bold"
        help_table_border: "${config.colors.bg0} None"
        preview_title: "${config.colors.white3} None / ${config.colors.white4} None"
        preview: "None ${config.colors.bg0}"
        preview_line_number: "${config.colors.primary} None / ${config.colors.primary} None"
        preview_match: "${config.colors.red0} ${config.colors.bg0} Bold"
        hex_null: "${config.colors.white4} None"
        hex_ascii_graphic: "${config.colors.white3} None"
        hex_ascii_whitespace: "${config.colors.cyan} None"
        hex_ascii_other: "${config.colors.green5} None"
        hex_non_ascii: "${config.colors.green5} None"
        staging_area_title: "${config.colors.white3} None / ${config.colors.white4} None"
        mode_command_mark: "${config.colors.white3} None"
        good_to_bad_0: "ansi(28)"
        good_to_bad_1: "ansi(29)"
        good_to_bad_2: "ansi(29)"
        good_to_bad_3: "ansi(29)"
        good_to_bad_4: "ansi(29)"
        good_to_bad_5: "ansi(100)"
        good_to_bad_6: "ansi(136)"
        good_to_bad_7: "ansi(172)"
        good_to_bad_8: "ansi(166)"
        good_to_bad_9: "ansi(196)"
    }
    ext_colors: {
        txt: "${config.colors.white3}"
        asy: "${config.colors.yellow1}"
        py: "${config.colors.cyan}"
        typ: "${config.colors.cyan}"
        hs: "${config.colors.magenta2}"
        vim: "${config.colors.green2}"
        nix: "${config.colors.magenta1}"
        rs: "${config.colors.orange1}"
        lua: "${config.colors.blue0}"
        c: "${config.colors.blue1}"
        r: "${config.colors.blue0}"
        pdf: "${config.colors.red2}"
        epub: "${config.colors.red2}"
        djvu: "${config.colors.red2}"
        lock: "${config.colors.white0}"
        torrent: "${config.colors.green1}"
        enc: "${config.colors.red1}"
    }
  '';

  # MPD configuration
  xdg.configFile."mpd/mpd.conf" = config.util.dotFileMut "mpd/mpd.conf";

  # Rpmc configuration
  xdg.configFile."rmpc/config.ron" = config.util.dotFileMut "rmpc/config.ron";
  xdg.configFile."rmpc/themes/ramak.ron" = config.util.dotFileMut "rmpc/theme.ron";

  home.stateVersion = "25.05";
}
