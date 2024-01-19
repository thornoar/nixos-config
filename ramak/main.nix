{ inputs, system, usrname, config, pkgs, lib, ... }:

{
    imports = [
        ../home-options.nix
        /etc/nixos/home-local.nix
        ../home-scripts.nix
        ./external.nix
    ];

    config = {
        home.username = usrname;
        home.homeDirectory = "/home/"+usrname;

        programs.nix-index.enable = true;

        nixpkgs.config.allowUnfree = true;
        home.packages = with pkgs; [
            moc
            mpv
            ncdu
            playerctl
            keynav
            sxiv
            xkb-switch
            btop
            iftop
            transmission
            brightnessctl
            acpi
            ghostscript
            pkgs.texlive.combined.scheme-full
            zathura
            ghc
            python3
            R
            manim
            bc
            neofetch
            file
            which
            tree
            gnused
            gnutar
            gawk
            zstd
            shellcheck
            nix-du
            sysstat
            lm_sensors
            ethtool
            glow
            thefuck
            bat
            tldr
            most
            libqalculate
            python311Packages.ipython
            eza
            ueberzug
            lazygit
            cheat
            toipe
            broot
            telegram-desktop
            discord
            flameshot
            obs-studio
            libnotify
            zoom-us
            xmobar
            xvkbd
            xwallpaper
            trash-cli
            goldendict-ng
            gource
            ripgrep
            xclip
            xsel
            fzf
        ];

        programs.zsh = {
            enable = true;
            enableCompletion = true;
            enableAutosuggestions = true;
            syntaxHighlighting.enable = true;
            shellAliases = rec {
                rb = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/";
                rb-home = "home-manager switch --impure --flake $NIXOS_CONFIG/";
                rb-system = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master";
                rb-boot = "sudo nixos-rebuild boot --impure --flake $NIXOS_CONFIG/#master";
                rb-both = "${rb}#master && ${rb-home}";
                fullrb = "${rb-both} && ${gc} && ${xc}";
                gc = "nix-collect-garbage --delete-old && sudo nix-collect-garbage --delete-old";
                xc = "xmonad --recompile && xmonad --restart";
                fullgc = "${gc} && ${rb-boot}";
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
