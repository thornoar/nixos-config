{ pkgs, lib, config, ... }:
{
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
  xdg.configFile."nvim/lua/colors.lua".text = config.util.toLua ''
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
        default: "${config.colors.white3} None / ${config.colors.white4} None"
        tree: "${config.colors.bg3} None / ${config.colors.bg2} None"
        parent: "${config.colors.yellow0} None / ${config.colors.yellow1} None"
        file: "${config.colors.fg0} None / ${config.colors.fg0} None"
        directory: "${config.colors.primary} None Bold / ${config.colors.primary} None"
        exe: "${config.colors.green1} None"
        link: "${config.colors.green1} None"
        pruning: "${config.colors.white4} None"
        perm__: "None None"
        perm_r: "${config.colors.yellow0} None"
        perm_w: "${config.colors.red1} None"
        perm_x: "${config.colors.orange0} None"
        owner: "${config.colors.white2} None"
        group: "${config.colors.white3} None"
        count: "${config.colors.white4} None"
        size: "${config.colors.white4} None"
        dates: "${config.colors.white3} None"
        sparse: "${config.colors.yellow0} None"
        content_extract: "ansi(29) None"
        content_match: "${config.colors.magenta0} None Bold"
        git_branch: "${config.colors.white2} None"
        git_insertions: "${config.colors.green4} None"
        git_deletions: "${config.colors.red1} None"
        git_status_current: "${config.colors.bg2} None"
        git_status_modified: "${config.colors.blue1} None"
        git_status_new: "${config.colors.green4} None Bold"
        git_status_ignored: "${config.colors.white3} None"
        git_status_conflicted: "${config.colors.red1} None"
        git_status_other: "${config.colors.red1} None"
        selected_line: "None ${config.colors.bg2} / None ${config.colors.bg2}"
        char_match: "${config.colors.magenta3} None"
        file_error: "${config.colors.red1} None"
        flag_label: "${config.colors.white4} None"
        flag_value: "${config.colors.magenta3} None Bold"
        input: "${config.colors.white2} None / ${config.colors.white4} None"
        status_error: "${config.colors.white3} ${config.colors.red0}"
        status_job: "${config.colors.yellow1} ${config.colors.bg1}"
        status_normal: "None ${config.colors.bg0} / None None"
        status_italic: "${config.colors.magenta3} ${config.colors.bg0} / None None"
        status_bold: "${config.colors.magenta3} ${config.colors.bg0} Bold / None None"
        status_code: "${config.colors.white2} ${config.colors.bg0} / None None"
        status_ellipsis: "${config.colors.white2} ${config.colors.bg0} Bold / None None"
        purpose_normal: "None None"
        purpose_italic: "${config.colors.magenta3} None"
        purpose_bold: "${config.colors.magenta3} None Bold"
        purpose_ellipsis: "None None"
        scrollbar_track: "${config.colors.bg0} None / ${config.colors.bg0} None"
        scrollbar_thumb: "${config.colors.white3} None / ${config.colors.white4} None"
        help_paragraph: "None None"
        help_bold: "${config.colors.orange1} None Bold"
        help_italic: "${config.colors.magenta3} None"
        help_code: "${config.colors.green4} ${config.colors.bg1}"
        help_headers: "${config.colors.orange1} None Bold"
        help_table_border: "${config.colors.bg0} None"
        preview_title: "${config.colors.white3} None / ${config.colors.white4} None"
        preview: "${config.colors.white3} ${config.colors.bg0} / ${config.colors.white3} ${config.colors.bg0}"
        preview_line_number: "${config.colors.white4} None / ${config.colors.white4} None"
        preview_match: "None ${config.colors.bg0} Bold"
        hex_null: "${config.colors.white4} None"
        hex_ascii_graphic: "${config.colors.white3} None"
        hex_ascii_whitespace: "${config.colors.orange0} None"
        hex_ascii_other: "${config.colors.orange1} None"
        hex_non_ascii: "${config.colors.orange1} None"
        staging_area_title: "${config.colors.white3} None / ${config.colors.white4} None"
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

  # # Bat configuration
  # programs.bat = {
  #   enable = true;
  #   extraPackages = with pkgs.bat-extras; [ batman ];
  # };
  # home.sessionVariables.BAT_THEME = "ansi";

  # Neofetch configuration
  xdg.configFile."neofetch/config.conf" = config.util.dotFileMut "neofetch.conf";

  # Khal configuration
  xdg.configFile."khal/config" = config.util.dotFileMut "khal.config.ini";

  # Tmux configuration
  xdg.configFile."tmux/tmux.conf" = config.util.dotFileMut "tmux/tmux.conf";

  # # Btop config
  # xdg.configFile."btop/btop.conf" = config.util.dotFileMut "btop/btop.conf";
  # xdg.configFile."btop/themes" = config.util.dotFileMut "btop/themes";

  # Htop config
  xdg.configFile."htop/htoprc" = config.util.dotFileMut "htop/htoprc";

  # Libraries configuration
  xdg.dataFile = builtins.listToAttrs (
    # Typst libraries
    lib.lists.flatten (lib.lists.forEach
      (lib.filesystem.listFilesRecursive ../src/typst-libraries)
      (filename:
        let
          strname = builtins.toString filename;
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

  home.packages = let
    readCustomPackages = file:
      lib.lists.forEach (pkgs.tools.readFile file)
      (x: pkgs.inputs.${x}.packages.${pkgs.system}.default);
    in with pkgs;
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
        vector
      ]))
      (texlive.combine { inherit (texlive) scheme-full; })
    ]
    # ++ (with pkgs.unstable; [ lua ])
    ++ pkgs.tools.readPackages ../src/packages/development.txt pkgs
    ++ readCustomPackages ../src/packages/custom.txt;
}
