{ pkgs, lib, config, ... }:
{
  # Vim/Neovim configuration
  programs.vim.enable = true;
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
  };

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
    pyright

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
    haskell-language-server

    # LaTeX
    (texlive.combine { inherit (texlive) scheme-full; })
    texlab

    # Typst
    typst
    tinymist

    # C/C++
    clang
    clang-tools
    glibc
    glibc.static
    cmake

    # Lua
    lua
    lua-language-server

    # # Go
    # go

    # # Julia
    # julia

    # Rust
    cargo
    rustc
    rust-analyzer
    evcxr

    # # Java
    # openjdk17
    # java-language-server

    # # Clojure
    # leiningen
    # clojure
    # clojure-lsp

    # Scala
    scala
    scala-cli
    metals
    sbt

    # Node packages
    nodePackages.typescript-language-server
    nodePackages.bash-language-server

    # Assembly
    nasm
    asm-lsp

    # Nix
    nixfmt-classic
    manix
    nix-output-monitor
    nvd
    nixd

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
