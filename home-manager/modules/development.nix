{ pkgs, lib, config, ... }:
{
  # Vim/Neovim configuration
  programs.vim.enable = true;
  programs.neovim = {
    enable = true;
    withPython3 = true;
    extraPython3Packages = ps: with ps; [ sympy pynvim ];
    extraPackages = with pkgs; [
      tree-sitter
      ripgrep
      fd
      haskellPackages.fast-tags
      haskellPackages.haskell-debug-adapter
      haskellPackages.ghci-dap
      pyright
      haskell-language-server
      tinymist
      lua-language-server
      rust-analyzer
      # java-language-server
      # clojure-lsp
      metals
      nodePackages.typescript-language-server
      nodePackages.bash-language-server
      asm-lsp
      nixd
    ];
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

  # haskeline config
  home.file.".haskeline" = config.util.dotFileMut "haskeline";

  # Asymptote configuration
  home.file.".asy/config.asy" = config.util.dotFileMut "config.asy";

  # R configuration
  home.file.".Rprofile" = config.util.dotFileMut "Rprofile";

  # GHCi configuration
  home.file.".ghci" = config.util.dotFileMut "ghci";

  xdg.configFile."cabal/config" = config.util.dotFileMut "cabal/config";

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
  };

  home.packages = (with pkgs; [
    # Python
    (python3.withPackages (ps: with ps; [
      manim ipython sympy numpy ollama openai mutagen
    ]))
    sage

    # R
    (rWrapper.override {
      packages = with rPackages; [
        languageserver ggplot2 dplyr xts pracma latex2exp
      ];
    })

    # Haskell
    (haskellPackages.ghcWithPackages (hspkgs: with hspkgs; [
      QuickCheck
      haskeline
      vector
    ]))
    cabal-install
    haskellPackages.cabal-clean
    haskellPackages.hoogle

    # LaTeX
    (texlive.combine { inherit (texlive) scheme-full; })
    texlab

    # Typst
    typst

    # C/C++
    clang
    clang-tools
    glibc
    glibc.static
    cmake

    # Lua
    lua5_1
    luarocks

    # # Go
    # go

    # # Julia
    # julia

    # Rust
    cargo
    rustc
    evcxr

    # Java and Android
    openjdk17
    # android-studio
    # android-studio-tools
    # androidenv.androidPkgs.androidsdk

    # # Clojure
    # leiningen
    # clojure

    # Scala
    scala
    scala-cli
    sbt

    # Node packages

    # Assembly
    nasm

    # Nix
    nixfmt-classic
    manix
    nix-output-monitor
    nvd

    # Coq
    coq

    # Ocaml
    ocaml

    # Other development tools
    libffi
    libtool
    cheat
    gnumake
    jq
    upx
    pkg-config
    pstree
    fzf
    shellcheck
    ghostscript
    pdf2svg
    nodejs
    pastel
    tealdeer
    coursier
    cloc
    tmux
    bc
  ]);
}
