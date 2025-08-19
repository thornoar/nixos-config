{ inputs, system, pkgs, lib, config, pkgs-unstable, readPackages, readFile, ... }:
let
  dotfile = str: lib.path.append ../src str;
  ts = builtins.toString;
  toLua = lib.attrsets.foldlAttrs (str: k: v:
    str + ''
      M.${k} = "${ts v}"
    '');
  readCustomPackages = file:
    lib.lists.forEach (readFile file)
    (x: inputs.${x}.packages.${system}.default);
in {
  imports = [
    ./scripts.nix
  ];

  # Neovim configuration
  programs.neovim = {
    enable = true;
    # package = pkgs-unstable.neovim;
    withPython3 = true;
    extraPython3Packages = ps: with ps; [ sympy pynvim ];
  };
  xdg.configFile."nvim/ftdetect".source = dotfile "nvim/ftdetect";
  xdg.configFile."nvim/syntax".source = dotfile "nvim/syntax";
  xdg.configFile."nvim/UltiSnips".source = dotfile "nvim/UltiSnips";
  xdg.configFile."nvim/after".source = dotfile "nvim/after";
  xdg.configFile."nvim/lua/setup".source = dotfile "nvim/lua/setup";
  xdg.configFile."nvim/lua/colors.lua".text = toLua ''
    local M = {}
  '' config.colors + ''
    return M
  '';
  xdg.configFile."nvim/init.lua".source = dotfile "nvim/init.lua";

  # Broot configuration
  xdg.configFile."broot/conf.hjson".source = dotfile "broot/conf.hjson";
  xdg.configFile."broot/verbs.hjson".source = dotfile "broot/verbs.hjson";
  xdg.configFile."broot/colorscheme.hjson".text = ''
    skin: {
        default: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
        tree: "${config.colors.bgColor3} None / ${config.colors.bgColor2} None"
        parent: "${config.colors.colorYellow0} None / ${config.colors.colorYellow1} None"
        file: "None None / None  None Italic"
        directory: "${config.colors.colorBlue1} None Bold / ${config.colors.colorBlue1} None"
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
        pdf: "${config.colors.colorRed2}"
        epub: "${config.colors.colorRed2}"
        djvu: "${config.colors.colorRed2}"
        lock: "${config.colors.colorWhite0}"
        torrent: "${config.colors.colorGreen1}"
    }
  '';

  # Asymptote configuration
  home.file.".asy/config.asy".source = dotfile "config.asy";

  # GHCi configuration
  home.file.".ghci".source = dotfile "ghci";

  # zsh configuration
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = false;
    syntaxHighlighting.enable = true;
    shellAliases = {
      torrent = "transmission-remote";
      film = "transmission-remote -w ~/media/films -a ";
      music = "transmission-remote -w ~/media/music -a ";
      lbr = "clear; br";
      open = "xdg-open";
      close = "exit";
      grep = "grep --color=auto";
      def = "dict -h dict.org";
      vmcon = "virt-manager --connect qemu:///system --show-domain-console";
      clip = "wl-copy -n";
      gpp = "g++ -std=c++11 -Wall -fsanitize=leak,address,undefined";
      gitlog = "git log --oneline --graph --decorate";
      # sudo = "doas";
    };
    envExtra = builtins.readFile (dotfile "zsh/envExtra.zsh");
    initContent = builtins.readFile (dotfile "zsh/initExtra.zsh");
  };
  xdg.configFile."nix-develop/.zshrc".text = ''
    source ~/.zshrc
    PS1="[%{$fg[magenta]%}develop%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "
  '';
  xdg.configFile."special-terminal/.zshrc".text = ''
    source ~/.zshrc
    PS1="[%{$fg[magenta]%}terminal%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "
  '';
  xdg.configFile."special-music/.zshrc".text = ''
    source ~/.zshrc
    PS1="[%{$fg[blue]%}music%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "
  '';

  # R configuration
  home.file.".Rprofile".source = dotfile "Rprofile";

  # neofetch configuration
  xdg.configFile."neofetch/config.conf".source = dotfile "neofetch.conf";

  # khal configuration
  xdg.configFile."khal/config".source = dotfile "khal.config.ini";

  # tmux configuration
  xdg.configFile."tmux/tmux.conf".source = dotfile "tmux/tmux.conf";

  # Libraries configuration
  xdg.dataFile = builtins.listToAttrs (
    # Typst libraries
    lib.lists.flatten (lib.lists.forEach
      (lib.filesystem.listFilesRecursive (dotfile "typst-libraries")) (filename:
        let
          strname = ts filename;
          last = lib.lists.last (lib.strings.splitString "/" strname);
          base = lib.lists.head (lib.strings.splitString "." last);
        in [
          {
            name = "typst/packages/local/" + base + "/0.0.0/main.typ";
            value = { source = strname; };
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
    "latex".source = dotfile "latex-libraries";
  };

  # pshash config
  xdg.configFile."pshash/pshash.conf".source = dotfile "pshash.conf";

  # haskeline config
  home.file.".haskeline".text = ''
    editMode: Vi
    historyDuplicates: IgnoreConsecutive
    bellStyle: NoBell
  '';

  programs = {
    git = {
      enable = true;
      userName = "Roman Maksimovich";
      userEmail = "r.a.maksimovich@gmail.com";
      extraConfig = { init.defaultBranch = "master"; };
      aliases = {
        install = ''
          !git clone "https://github.com/thornoar/$1.git" "$HOME/projects/$1"'';
      };
    };
    gh = { enable = true; };
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
      ]))
      (texlive.combine { inherit (texlive) scheme-full; })
    ]
    ++ readPackages ../src/packages/development.txt pkgs
    ++ readPackages ../src/packages/unstable.txt pkgs-unstable
    ++ readCustomPackages ../src/packages/custom.txt;
}
