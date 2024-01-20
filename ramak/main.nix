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

        # home.file.".zshenv".text = ''
        # '';

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
