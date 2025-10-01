{ pkgs, lib, config, ... }:
let
  ts = builtins.toString;
  toLua = lib.attrsets.foldlAttrs (str: k: v:
    str + ''
      M.${k} = "${ts v}"
    ''
  );
  readCustomPackages = file:
    lib.lists.forEach (pkgs.readFile file)
    (x: pkgs.inputs.${x}.packages.${pkgs.system}.default);
in {
  # Vim/Neovim configuration
  programs.vim.enable = true;
  # home.file.".vimrc".text = "set number";
  programs.neovim = {
    enable = true;
    withPython3 = true;
    extraPython3Packages = ps: with ps; [ sympy pynvim ];
  };
  xdg.configFile."nvim/ftdetect" = config.util.dotFileMut "nvim/ftdetect";
  xdg.configFile."nvim/syntax" = config.util.dotFileMut "nvim/syntax";
  xdg.configFile."nvim/UltiSnips" = config.util.dotFileMut "nvim/UltiSnips";
  xdg.configFile."nvim/after" = config.util.dotFileMut "nvim/after";
  xdg.configFile."nvim/lua/setup" = config.util.dotFileMut "nvim/lua/setup";
  xdg.configFile."nvim/lua/colors.lua".text = toLua ''
    local M = {}
  '' config.colors + ''
    return M
  '';
  xdg.configFile."nvim/init.lua" = config.util.dotFileMut "nvim/init.lua";

  # Broot configuration
  xdg.configFile."broot/conf.hjson" = config.util.dotFileMut "broot/conf.hjson";
  xdg.configFile."broot/verbs.hjson" = config.util.dotFileMut "broot/verbs.hjson";
  xdg.configFile."broot/colorscheme.hjson".text = ''
    skin: {
        default: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
        tree: "${config.colors.bgColor3} None / ${config.colors.bgColor2} None"
        parent: "${config.colors.colorYellow0} None / ${config.colors.colorYellow1} None"
        file: "${config.colors.fgColor0} None / ${config.colors.fgColor0} None"
        directory: "${config.colors.primary} None Bold / ${config.colors.primary} None"
        exe: "${config.colors.colorGreen1} None"
        link: "${config.colors.colorGreen1} None"
        pruning: "${config.colors.colorWhite4} None"
        perm__: "None None"
        perm_r: "${config.colors.colorYellow0} None"
        perm_w: "${config.colors.colorRed1} None"
        perm_x: "${config.colors.colorOrange0} None"
        owner: "${config.colors.colorWhite2} None"
        group: "${config.colors.colorWhite3} None"
        count: "${config.colors.colorWhite4} None"
        size: "${config.colors.colorWhite4} None"
        dates: "${config.colors.colorWhite3} None"
        sparse: "${config.colors.colorYellow0} None"
        content_extract: "ansi(29) None"
        content_match: "${config.colors.colorMagenta0} None Bold"
        git_branch: "${config.colors.colorWhite2} None"
        git_insertions: "${config.colors.colorGreen4} None"
        git_deletions: "${config.colors.colorRed1} None"
        git_status_current: "${config.colors.bgColor2} None"
        git_status_modified: "${config.colors.colorBlue1} None"
        git_status_new: "${config.colors.colorGreen4} None Bold"
        git_status_ignored: "${config.colors.colorWhite3} None"
        git_status_conflicted: "${config.colors.colorRed1} None"
        git_status_other: "${config.colors.colorRed1} None"
        selected_line: "None ${config.colors.bgColor2} / None ${config.colors.bgColor2}"
        char_match: "${config.colors.colorMagenta3} None"
        file_error: "${config.colors.colorRed1} None"
        flag_label: "${config.colors.colorWhite4} None"
        flag_value: "${config.colors.colorMagenta3} None Bold"
        input: "${config.colors.colorWhite2} None / ${config.colors.colorWhite4} None"
        status_error: "${config.colors.colorWhite3} ${config.colors.colorRed0}"
        status_job: "${config.colors.colorYellow1} ${config.colors.bgColor1}"
        status_normal: "None ${config.colors.bgColor0} / None None"
        status_italic: "${config.colors.colorMagenta3} ${config.colors.bgColor0} / None None"
        status_bold: "${config.colors.colorMagenta3} ${config.colors.bgColor0} Bold / None None"
        status_code: "${config.colors.colorWhite2} ${config.colors.bgColor0} / None None"
        status_ellipsis: "${config.colors.colorWhite2} ${config.colors.bgColor0} Bold / None None"
        purpose_normal: "None None"
        purpose_italic: "${config.colors.colorMagenta3} None"
        purpose_bold: "${config.colors.colorMagenta3} None Bold"
        purpose_ellipsis: "None None"
        scrollbar_track: "${config.colors.bgColor0} None / ${config.colors.bgColor0} None"
        scrollbar_thumb: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
        help_paragraph: "None None"
        help_bold: "${config.colors.colorOrange1} None Bold"
        help_italic: "${config.colors.colorMagenta3} None"
        help_code: "${config.colors.colorGreen4} ${config.colors.bgColor1}"
        help_headers: "${config.colors.colorOrange1} None Bold"
        help_table_border: "${config.colors.bgColor0} None"
        preview_title: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
        preview: "${config.colors.colorWhite3} ${config.colors.bgColor0} / ${config.colors.colorWhite3} ${config.colors.bgColor0}"
        preview_line_number: "${config.colors.colorWhite4} None / ${config.colors.colorWhite4} None"
        preview_match: "None ${config.colors.bgColor0} Bold"
        hex_null: "${config.colors.colorWhite4} None"
        hex_ascii_graphic: "${config.colors.colorWhite3} None"
        hex_ascii_whitespace: "${config.colors.colorOrange0} None"
        hex_ascii_other: "${config.colors.colorOrange1} None"
        hex_non_ascii: "${config.colors.colorOrange1} None"
        staging_area_title: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
        mode_command_mark: "gray(5) ansi(204) Bold"
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
        txt: "${config.colors.colorWhite3}"
        asy: "${config.colors.colorYellow1}"
        py: "${config.colors.colorCyan}"
        typ: "${config.colors.colorCyan}"
        hs: "${config.colors.colorMagenta2}"
        vim: "${config.colors.colorGreen2}"
        nix: "${config.colors.colorMagenta1}"
        rs: "${config.colors.colorOrange1}"
        lua: "${config.colors.colorBlue0}"
        c: "${config.colors.colorBlue1}"
        r: "${config.colors.colorBlue0}"
        pdf: "${config.colors.colorRed2}"
        epub: "${config.colors.colorRed2}"
        djvu: "${config.colors.colorRed2}"
        lock: "${config.colors.colorWhite0}"
        torrent: "${config.colors.colorGreen1}"
        enc: "${config.colors.colorRed1}"
    }
  '';

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

  # Bat configuration
  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [ batman ];
  };
  home.sessionVariables.BAT_THEME = "ansi";

  # neofetch configuration
  xdg.configFile."neofetch/config.conf" = config.util.dotFileMut "neofetch.conf";

  # khal configuration
  xdg.configFile."khal/config" = config.util.dotFileMut "khal.config.ini";

  # tmux configuration
  xdg.configFile."tmux/tmux.conf" = config.util.dotFileMut "tmux/tmux.conf";

  # Libraries configuration
  xdg.dataFile = builtins.listToAttrs (
    # Typst libraries
    lib.lists.flatten (lib.lists.forEach
      (lib.filesystem.listFilesRecursive ../src/typst-libraries)
      (filename:
        let
          strname = ts filename;
          last = lib.lists.last (lib.strings.splitString "/" strname);
          base = lib.lists.head (lib.strings.splitString "." last);
        in [
          {
            name = "typst/packages/local/" + base + "/0.0.0/main.typ";
            value = config.util.dotFileMut ("typst-libraries/" + builtins.baseNameOf filename);
          }
          {
            name = "typst/packages/local/" + base + "/0.0.0/typst.toml";
            value = {
              text = ''
                [package]
                name = "${base}"
                version = "0.0.0"
                entrypoint = "main.typ"
                authors = ["Roman Maksimovich"]
              '';
            };
          }
        ]
      )
    )
  ) // {
    # LaTeX libraries
    "latex" = config.util.dotFileMut "latex-libraries";
  };

  home.file = builtins.listToAttrs (
    lib.lists.forEach
      (lib.filesystem.listFilesRecursive ../src/scripts)
      (filename: {
          name = ".local/bin/" + (
            lib.strings.head (
              lib.strings.splitString "." (
                lib.lists.last
                (lib.strings.splitString "/" (builtins.toString filename))
              )
            )
          );
          value = config.util.dotFileMut ("scripts/" + builtins.baseNameOf filename);
        }
      )
  ) // {
    # haskeline config
    ".haskeline" = config.util.dotFileMut "haskeline";

    # Asymptote configuration
    ".asy/config.asy" = config.util.dotFileMut "config.asy";

    # R configuration
    ".Rprofile" = config.util.dotFileMut "Rprofile";

    # GHCi configuration
    ".ghci" = config.util.dotFileMut "ghci";

    # w3m config
    ".w3m/keymap" = config.util.dotFileMut "w3m.keymap";
  };

  # Pshash config
  xdg.configFile."pshash/pshash.conf" = config.util.dotFileMut "pshash.conf";

  # Cheat config
  xdg.configFile."cheat/conf.yml" = config.util.dotFileMut "cheat.conf.yml";

  programs = {
    git = {
      enable = true;
      userName = "Roman Maksimovich";
      userEmail = "r.a.maksimovich@gmail.com";
      extraConfig = {
        init.defaultBranch = "master";
      };
      aliases = {
        install = ''
          !git clone "https://github.com/thornoar/$1.git" "$HOME/projects/$1"'';
      };
    };
    gh = { enable = true; };
    nh = {
      enable = true;
      flake = "$NIXOS_CONFIG";
    };
  };

  home.packages = with pkgs;
    [
      (python3.withPackages (ps: with ps; [
          manim ipython sympy numpy ollama openai
        ]
      ))
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
        haskeline
      ]))
      (texlive.combine { inherit (texlive) scheme-full; })
    ]
    # ++ (with pkgs.unstable; [ lua ])
    ++ readPackages ../src/packages/development.txt pkgs
    ++ readCustomPackages ../src/packages/custom.txt;
}
