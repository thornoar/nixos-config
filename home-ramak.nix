{ inputs, system, usrname, config, pkgs, lib, ... }:

{
    imports = [
        ./home-options.nix
        /etc/nixos/home-local.nix
        ./home-scripts.nix
    ];

    config = {
        home.username = usrname;
        home.homeDirectory = "/home/"+usrname;

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
            brightnessctl
            acpi

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

            telegram-desktop
            discord
            flameshot
            obs-studio
            libnotify
            zoom-us

            xmobar
            # xdotool
            xvkbd
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
            };    
            initExtra = ''
                autoload -U colors && colors
                PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "
                eval $(thefuck --alias)
            '';
        };

        programs.git = {
            enable = true;
            userName = "Roman Maksimovich";
            userEmail = "r.a.maksimovich@gmail.com";
        };

        programs.firefox = {
            enable = true;
            profiles = {
                default = {
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
                    };
                    extensions = with inputs.firefox-addons.packages.${system}; [
                        darkreader
                        vimium
                    ];
                    bookmarks = [
                        {
                            name = "yt | YouTube";
                            url = "https://youtube.com";
                            keyword = "yt";
                        }
                        {
                            name = "ym | YouTube Music";
                            url = "https://music.youtube.com";
                            keyword = "ym";
                        }
                        {
                            name = "gh | GitHub";
                            url = "https://github.com/thornoar";
                            keyword = "gh";
                        }
                        {
                            name = "qw | Work Mail";
                            url = "https://mail.google.com/mail/u/1/#inbox";
                            keyword = "qw";
                        }
                        {
                            name = "as | Personal Mail";
                            url = "https://mail.google.com/mail/u/0/#inbox";
                            keyword = "as";
                        }
                        {
                            name = "mn | MyNixOS";
                            url = "https://mynixos.com";
                            keyword = "mn";
                        }
                        {
                            name = "tr | RuTracker";
                            url = "https://rutracker.org";
                            keyword = "tr";
                        }
                        {
                            name = "li | LibGen";
                            url = "https://libgen.is";
                            keyword = "li";
                        }
                        {
                            name = "ni | Nixhub.io";
                            url = "https://www.nixhub.io/";
                            keyword = "ni";
                        }
                    ];
                    search = {
                        force = true;
                        default = "Google";
                        order = [ "Google" "Searx" ];
                    };
                    userChrome = ''
                        #unified-extensions-button, #unified-extensions-button > .toolbarbutton-icon{
                        width: 0px !important;
                        padding: 0px !important;
                        }

                        .titlebar-buttonbox-container{ display:none }

                        #back-button, #forward-button { display:none!important; }

                        #PanelUI-button { display:none!important; }

                        #alltabs-button {
                            display: none !important;
                        }

                        #urlbar ::-moz-selection,
                        .searchbar-textbox ::-moz-selection {
                        background-color: #0078d7 !important;
                        color: #fff !important;
                        }
                    '';
                };
            };
        };

        programs.gh = { enable = true; };

        # asymptote setup
        home.file.".asy/config.asy".source = ./dotfiles/config.asy;

        # moc setup
        home.file.".moc/config".text = ''
            Theme = nightly_theme
            Keymap = keymap
            Repeat = yes
            #Shuffle = no
            AutoNext = no
        '';
        home.file.".moc/keymap".text = ''
            go    = ENTER RIGHT
            go_up = U LEFT
            #seek_forward  = RIGHT
            #seek_backward = LEFT
        '';

        # R setup
        home.file.".Rprofile".source = ./dotfiles/Rprofile;

        home.file.".xmobarrc".text = config.xmobarOptions;

        # neofetch setup
        xdg.configFile."neofetch/config.conf".source = ./dotfiles/neofetch.conf;


        # ranger setup
        xdg.configFile."ranger/rc.conf".text = ''
            set preview_images_method ueberzug
            set show_hidden false
            map ;h cd /home/${usrname}/
            map ;p cd /home/${usrname}/projects/
            map ;l cd /home/${usrname}/.config
            map ;e cd /etc/
            map ;m cd /home/${usrname}/media
            map ;c cd /home/${usrname}/.config/nvim
            map ;k cd /run/media/${usrname}
            map ;d cd /home/${usrname}/Downloads
        '';

        # keynav setup
        services.keynav.enable = true;
        xdg.configFile."keynav/keynavrc".source = ./dotfiles/keynavrc;

        xresources.properties = {
            "Xcursor.size" = 16;
            "Xcursor.theme" = "Adwaita";
        };
        home.stateVersion = "23.11";
        programs.home-manager.enable = true;
    };
}
