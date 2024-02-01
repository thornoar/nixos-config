{ inputs, system, usrname, config, pkgs, lib, ... }:

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

        programs.nix-index.enable = true;

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
            # gnused
            # gnutar
            # ueberzug
            # glow
            # lm_sensors
            # ethtool
            # zstd
            # tree
            # bc
            # acpi
            # brightnessctl

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

            # languages
            ghostscript
            pkgs.texlive.combined.scheme-full
            ghc
            R
            cargo
            rustc
            (python3.withPackages my-python-packages)
            manim

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
                # rb-boot = "sudo nixos-rebuild boot --impure --flake $NIXOS_CONFIG/#master";
                # fullgc = "${gc} && ${rb-boot}";
                rb = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/";
                hrb = "home-manager switch --impure --flake $NIXOS_CONFIG/; recompile_xmonad && xmonad --restart";
                srb = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master";
                brb = "${rb}#master && ${hrb}";
                frb = "${brb} && ${gc} && ${xc}";
                gc = "nix-collect-garbage --delete-old && sudo nix-collect-garbage --delete-old";
                xc = "xmonad --recompile && xmonad --restart";
                hm = "home-manager";
                gitpush = "git add . && git commit -m '--' && git push";
                gitpull = "git fetch && git pull";
                gst = "git status";
                ns = "nix-shell --command zsh -p ";
                cdir = "cd ~/.config/nvim";
                sbdir = "cd ~/projects/sandbox";
                media = "cd ~/media";
                films = "cd ~/media/Films";
                books = "cd ~/media/Books";
                wget = "wget --hsts-file = $XDG_DATA_HOME/wget-hsts";
                cp = "cp -i";
                tr-remote = "transmission-remote";
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
            };    
            initExtra = ''
                autoload -U colors && colors
                PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "
                source $NIXOS_CONFIG/dotfiles/shell-billy.sh
                source $NIXOS_CONFIG/dotfiles/br.sh
                eval $(thefuck --alias)
            '';
        };

        programs.git = {
            enable = true;
            userName = "Roman Maksimovich";
            userEmail = "r.a.maksimovich@gmail.com";
        };

        programs.gh = { enable = true; };

        xresources.properties = {
            "Xcursor.size" = 16;
            "Xcursor.theme" = "Adwaita";
        };
        home.stateVersion = "23.11";
        programs.home-manager.enable = true;
    };
}
