{ system, usrname, inputs, config, pkgs, lib, ... }:

{
    imports = [
        ../home-options.nix
        /etc/nixos/home-local.nix
        ../home-scripts.nix
        ./external.nix
    ];

    config = 
    let 
        my-python-packages = ps: with ps; [
            ipython
            sympy
            numpy
            pandas
            matplotlib
        ];
    in
    {
        home.username = usrname;
        home.homeDirectory = "/home/"+usrname;
        xdg.userDirs = {
            enable = true;
            download = "${config.home.homeDirectory}/dls";
            documents = "${config.home.homeDirectory}/docs";
        };

        xdg.mimeApps = {
            enable = true;
            associations.added = {
                "application/pdf" = ["org.pwmt.zathura-pdf-mupdf.desktop"];
                "audio/mpeg" = ["mpv.desktop"];
                "audio/mp3" = ["mpv.desktop"];
            };
            defaultApplications = {
                "application/pdf" = ["org.pwmt.zathura-pdf-mupdf.desktop"];
                "audio/mpeg" = ["mpv.desktop"];
                "audio/mp3" = ["mpv.desktop"];
            };
        };

        nixpkgs.config.allowUnfree = true;
        home.packages = with pkgs; [
            # terminal utilities
            playerctl
            keynav
            trash-cli
            ripgrep
            xclip
            xsel
            fzf
            imagemagick
            ffmpeg
            neofetch
            file
            which
            transmission
            gawk
            shellcheck
            nix-du
            sysstat
            thefuck
            bat
            tldr
            most
            eza
            cheat
            xcolor
            killall
            dict
            gifgen

            # terminal applications
            broot
            yazi
            moc
            ncdu
            btop
            iftop
            lazygit
            toipe
            libqalculate
            graphviz

            # languages
            ghostscript
            pkgs.texlive.combined.scheme-full
            ghc
            R
            cargo
            rustc
            (python3.withPackages my-python-packages)
            manim
            lua

            # GUI applications
            telegram-desktop
            discord
            flameshot
            obs-studio
            zoom-us
            goldendict-ng
            mpv
            zathura
            sxiv
            gource
            audacity

            # Desktop
            xkb-switch
            libnotify
            xmobar
            xvkbd
            hsetroot
        ];

        programs.zsh = {
            enable = true;
            enableCompletion = true;
            enableAutosuggestions = true;
            syntaxHighlighting.enable = true;
            shellAliases = rec {
                rc = "nmcli con up 9a64ee51-26a1-4c77-9361-df3de07cbfab";
                xc = "xmonad --recompile && xmonad --restart";
                hm = "home-manager";
                gst = "git status";
                ns = "nix-shell --command zsh -p ";
                cdir = "cd ~/.config/nvim";
                sbdir = "cd ~/projects/sandbox";
                media = "cd ~/media";
                films = "cd ~/media/films";
                books = "cd ~/media/books";
                wget = "wget --hsts-file = $XDG_DATA_HOME/wget-hsts";
                cp = "cp -i";
                trr = "transmission-remote";
                film = "transmission-remote -w ~/media/films -a ";
                music = "transmission-remote -w ~/media/music -a ";
                c = "ping google.com";
                la = "exa -lAh";
                open = "xdg-open";
                svim = "sudo -E nvim";
                sc = "cd $NIXOS_CONFIG";
                calc = "qalc -c";
                quit = "exit";
                grep = "grep --color=auto";
                fucking = "mommy";
                q = "qalc -c";
                sxiv = "sxiv -b";
                lg = "lazygit";
                ip = "ipython";
                def = "dict -h dict.org";
            };    
            initExtra = ''
                autoload -U colors && colors
                PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "
                source $NIXOS_CONFIG/dotfiles/shell-billy.sh
                source $NIXOS_CONFIG/dotfiles/br.sh
                eval $(thefuck --alias)

                export ATUIN_NOBIND="true"
                eval "$(atuin init zsh)"

                # bindkey '^z' atuin-search

                # bind to the up key, which depends on terminal mode
                bindkey '^[[1;5A' atuin-up-search
                # bindkey '^[OA' atuin-up-search
            '';
        };

        programs.git = {
            enable = true;
            userName = "Roman Maksimovich";
            userEmail = "r.a.maksimovich@gmail.com";
        };

        programs.gh = { enable = true; };

        programs.atuin = {
            enable = true;
            enableZshIntegration = true;
            flags = [
              "--disable-up-arrow"
            ];
        };

        programs.zoxide = {
            enable = true;
            enableZshIntegration = true;
        };

        xresources.properties = {
            "Xcursor.size" = 16;
            "Xcursor.theme" = "Adwaita";
        };
        home.stateVersion = "23.11";
        programs.home-manager.enable = true;
    };
}
