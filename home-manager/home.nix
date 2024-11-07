{ config, pkgs, firefox-pkgs, lib, pkgs-unstable, ... }:

{
    imports = (
        let
            path = /home/ramak/projects/nixos-local-config/home-local.nix;
        in if (builtins.pathExists path) then [ path ] else [ ./src/home-template.nix ]
    ) ++ [
        ./modules/options.nix
        ./modules/scripts.nix
        ./modules/external-smart.nix
        ./modules/external-direct.nix
    ];

    config = 
    let 
        # [./modules/packages.txt]
        regular-packages = with pkgs; [
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
            ghc
            cabal-install
            haskellPackages.cabal-clean
            haskell-language-server

            # Lua
            lua
            lua-language-server

            # Julia
            julia

            # Rust
            cargo
            rustc
            rust-analyzer

            # R
            (rWrapper.override {
                packages = with rPackages; [
                    languageserver ggplot2 dplyr xts pracma
                ];
            })

            # Java
            # openjdk

            # Typst
            typst
            typst-lsp

            # Nix
            # nil
            nixd
            alejandra

            xorg.xcursorgen
            hyprcursor
            xcur2png
        ];
        unstable-packages = with pkgs-unstable; [
            khal
            fzf
            nodejs
        ];
        insecure-packages = with pkgs; [
            sc-im
        ];
    in
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
        home.sessionVariables = {
            BAT_THEME = "base16";
        };

        xdg.mimeApps = rec {
            enable = true;
            associations.added = {
                "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
                "audio/mpeg" = [ "mpv.desktop" ];
                "audio/mp3" = [ "mpv.desktop" ];
                "video/vnd.avi" = [ "mpv.desktop" ];
                "image/vnd.djvu+multipage" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
                "image/svg+xml" = [ "sxiv.desktop" ];
                "text/csv" = [ "sc-im.desktop" ];
            };
            defaultApplications = associations.added;
        };
        nixpkgs.config.allowUnfree = true;

        home.packages = (
            if config.misc.usePackageList then (
                lib.lists.forEach (
                    lib.lists.partition
                        (x: 0 < lib.strings.stringLength x) 
                        (lib.strings.splitString "\n" (builtins.readFile ./modules/packages.txt))
                ).right (name: pkgs.${name})
            ) else []
		) ++ regular-packages ++ unstable-packages ++ insecure-packages;

        programs = {
            neovim = {
                enable = true;
            };
            hmd = {
                enable = true;
                runOnSwitch = false;
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
            firefox = {
                enable = true;
                profiles.default = {
                    id = 0;
                    name = "default";
                    isDefault = true;
                    settings = {
                        "browser.startup.homepage" = "about:home";
                        "browser.tabs.inTitlebar" = 0;
                        "browser.toolbars.bookmarks.visibility" = "never";
                        "browser.search.defaultenginename" = "Google";
                        "browser.search.order.1" = "Google";
                        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
                        "signon.rememberSignons" = false;
                        "media.hardware-video-decoding.enabled" = true;
                        "layout.css.devPixelsPerPx" = 1.7;
                        "layout.css.dpi" = 96;
                    };
                    extensions = with firefox-pkgs; [
                        darkreader
                        vimium
                        adblocker-ultimate
                    ];
                    search = {
                        force = true;
                        default = "Google";
                        order = [ "Google" "Searx" ];
                    };
                };
            };
            home-manager = {
                enable = true;
            };
        };

        home.pointerCursor = {
            x11.enable = true;
            gtk.enable = true;
            name = "Adwaita";
            package = pkgs.gnome.adwaita-icon-theme;
        };

        dconf.settings = {
            "org/virt-manager/virt-manager/connections" = {
                autoconnect = ["qemu:///system"];
                uris = ["qemu:///system"];
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

        # Keynav service
        services.keynav.enable = true;

        # Enabling insecure library
        nixpkgs.config = {
            permittedInsecurePackages = [
                "libxls-1.6.2"
            ];
        };

        home.stateVersion = "23.11";
    };
}
