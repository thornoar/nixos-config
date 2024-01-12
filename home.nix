{ system, config, pkgs, lib, inputs, ... }:

{
    options = {
        fontsize = lib.mkOption {
            type = lib.types.int;
            default = 11;
        };
    };

    imports = [
        /etc/nixos/home-options.nix
        ./modules/home-terminal.nix
        ./modules/browser.nix
        ./modules/scripts.nix
    ];

    config = {
        home.username = "ramak";
        home.homeDirectory = "/home/ramak";

        programs.nix-index.enable = true;

        nixpkgs.config.allowUnfree = true;
        home.packages = with pkgs; [
            ranger
            moc
            mpv
            w3m
            feh
            ncdu
            playerctl
            keynav
            sxiv
            xkb-switch
            btop
            iftop
            transmission

            ghostscript
            pkgs.texlive.combined.scheme-full
            zathura
            ghc
            python3
            R

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
            
            glow
            thefuck

            telegram-desktop
            discord
            flameshot
        ];

        programs.git = {
            enable = true;
            userName = "Roman Maksimovich";
            userEmail = "r.a.maksimovich@gmail.com";
        };

        programs.gh = { enable = true; };

        # asymptote setup
        home.file.".asy/config.asy".source = ./dotfiles/config.asy;

        # moc setup
        home.file.".moc/config".text = ''
            Theme = nightly_theme
            Keymap = keymap
            #Repeat = no
            #Shuffle = no
            #AutoNext = yes
        '';
        home.file.".moc/keymap".text = ''
            go    = ENTER RIGHT
            go_up = U LEFT
            #seek_forward  = RIGHT
            #seek_backward = LEFT
        '';

        # R setup
        home.file.".Rprofile".source = ./dotfiles/Rprofile;

        # xmonad setup
        xdg.configFile."xmonad".source = ./dotfiles/xmonad;

        # neofetch setup
        xdg.configFile."neofetch/config.conf".source = ./dotfiles/neofetch/config.conf;

        # zathura setup
        xdg.configFile."zathura".source = ./dotfiles/zathura;

        # ranger setup
        xdg.configFile."ranger/rc.conf".text = ''
            set preview_images_method ueberzug
            set show_hidden false
            map ;h cd /home/ramak/
            map ;p cd /home/ramak/projects/
            map ;l cd /home/ramak/.config
            map ;e cd /etc/
            map ;m cd /home/ramak/media
            map ;c cd /home/ramak/.config/nvim
            map ;k cd /run/media/ramak
            map ;d cd /home/ramak/Downloads
        '';

        # keynav setup
        services.keynav.enable = true;
        xdg.configFile."keynav/keynavrc".source = ./dotfiles/keynav/keynavrc;

        xresources.properties = {
            "Xcursor.size" = 16;
            "Xcursor.theme" = "Adwaita";
        };
        home.stateVersion = "23.11";
        programs.home-manager.enable = true;
    };
}
