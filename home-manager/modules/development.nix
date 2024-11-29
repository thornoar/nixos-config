{ pkgs, ... }:
{
    home.packages =  with pkgs; [
        # LaTeX
        (texlive.combine { inherit (texlive) scheme-full; })
        # (texlive.combine { inherit (texlive) scheme-basic dvisvgm dvipng amsmath latexmk lipsum; })
        texlab

        # Asymptote
        # asymptote

        # C
        clang
        clang-tools

        # Python
        (python3.withPackages (ps: with ps; [
            manim ipython sympy numpy
        ]))
        pyright

        # Haskell
        cabal-install
        haskellPackages.cabal-clean
        haskell-language-server
        haskell.compiler.ghc965

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
        rust-analyzer
        evcxr

        # R
        (rWrapper.override {
            packages = with rPackages; [
                languageserver ggplot2 dplyr xts pracma
            ];
        })

        # Java
        openjdk
        # java-language-server

        # Typst
        typst
        typst-lsp

        # Nix
        # nil
        nixd
        alejandra

        # JavaScript
        nodePackages.typescript-language-server

        # Bash
        nodePackages.bash-language-server

        # Clojure
        clojure
        clojure-lsp

        # Prolog
        swiProlog

        # Sage
        sage

        # Tmux
        tmux

        # xorg.xcursorgen
        # hyprcursor
        # xcur2png
    ];

    programs = {
        # neovim = {
        #     enable = true;
        # };
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
                # clip = "xclip -r -selection c";
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
