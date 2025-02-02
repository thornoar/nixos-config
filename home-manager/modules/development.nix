{ pkgs, pkgs-unstable, ... }:
{
    home.packages =  with pkgs; [
        # LaTeX
        (texlive.combine { inherit (texlive) scheme-medium; })
        # (texlive.combine { inherit (texlive) scheme-basic dvisvgm dvipng amsmath latexmk lipsum asymptote tikz; })
        texlab

        # Asymptote
        # asymptote

        # C
        clang
        clang-tools

        # Python
        (python3.withPackages (ps: with ps; [
            manim ipython sympy numpy ollama openai
        ]))
        pyright

        # Haskell
        cabal-install
        haskellPackages.cabal-clean
        haskell-language-server
        haskell.compiler.ghc966

        # Lua
        lua
        luarocks
        lua-language-server

        # Go
        go

        # Julia
        julia

        # Rust
        cargo
        rustc
        clippy
        rust-analyzer
        evcxr

        # R
        (rWrapper.override {
            packages = with rPackages; [
                languageserver ggplot2 dplyr xts pracma latex2exp
            ];
        })

        # Java
        openjdk17
        java-language-server

        # Typst
        # typst
        # tinymist
        # typst-lsp

        # Nix
        nixd
        alejandra

        # JavaScript
        nodePackages.typescript-language-server

        # Bash
        nodePackages.bash-language-server

        # Lisp
        clojure
        clojure-lsp
        leiningen

        # Prolog
        swi-prolog

        # Sage
        sage

        # Tmux
        tmux

        # Kotlin
        # kotlin
        # kotlin-interactive-shell
        # kotlin-language-server

        # Assembly
        nasm

        # android-studio
        flutter
    ];

    programs = {
        neovim = {
            enable = true;
            withPython3 = true;
            # package = pkgs-unstable.neovim;
            extraPython3Packages = ps: with ps; [ sympy pynvim ];
        };
        helix = {
            enable = true;
            package = pkgs-unstable.helix;
        };
        zsh = {
            enable = true;
            enableCompletion = true;
            autosuggestion.enable = true;
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
                vmstart = "sudo virsh start";
                vmstop = "sudo virsh shutdown";
            };    
        };
        git = {
            enable = true;
            userName = "Roman Maksimovich";
            userEmail = "r.a.maksimovich@gmail.com";
            extraConfig = {
                init.defaultBranch = "master";
            };
        };
        gh = { enable = true; };
    };
}
