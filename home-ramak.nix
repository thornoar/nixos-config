{ inputs, usrname, config, pkgs, lib, ... }:

{
    imports = [
        /etc/nixos/home-options.nix
        ./modules/home-terminal.nix
        ./modules/browser.nix
        ./modules/scripts.nix
    ];

    options = 
    let opt = lib.mkOption; tp = lib.types; in
    {
        fontsize = opt {
            type = tp.int;
        };
        fontsizeBar = opt {
            type = tp.int;
        };
        wallpaperDir = opt {
            type = tp.str;
            default = "Landscapes";
        };
        windowSpace = opt {
            type = tp.int;
            default = 5;
        };
        bgColor = opt {
            type = tp.str;
            default = "#1e2127";
            # default = "#000000";
        };
        fgColor = opt {
            type = tp.str;
            default = "#17a88b";
        };
        brfgColor = opt {
            type = tp.str;
            default = "#00bc96";
        };
        colorWhite = opt {
            type = tp.str;
            default = "#f8f8f2";
        };
        colorBlue = opt {
            type = tp.str;
            default = "#bd93f9" ;
        };
        colorBlue_alt = opt {
            type = tp.str;
            default = "#61afef";
        };
        colorMagenta = opt {
            type = tp.str;
            default = "#ff79c6";
        };
        colorMagenta_alt = opt {
            type = tp.str;
            default = "#c678dd";
            # default = config.colorBlue;
        };
        font = opt {
            type = tp.str;
            default = "Hack";
        };
        padding = opt {
            type = tp.attrs;
            default = { x = 6; y = 6; };
        };
        barheight = opt {
            type = tp.int;
            default = 35;
        };
        xmonadLayouts = opt {
            type = tp.str;
            default = "tall ||| Full ||| magnified ||| tabs ||| spirals";
        };
        xmobarOptions = opt {
            type = tp.str;
            default = ''
                Config { font     = "xft:${config.font} Nerd Font Mono-${builtins.toString config.fontsizeBar}"
                       , bgColor  = "${config.bgColor}"
                       , fgColor  = "${config.fgColor}"
                       , position = TopH ${builtins.toString config.barheight}
                       , persistent = False
                       , hideOnStart = False
                       , allDesktops = True
                       , lowerOnStart = True
                       , commands = [
                                    Run Alsa "default" "Master"
                                        [ "--template", "<fc=${config.colorWhite}><volumestatus></fc>"
                                        , "--suffix"  , "True"
                                        , "--"
                                        , "--on", ""
                                    ]
                                    , Run Date "<fc=${config.colorMagenta}>%H:%M:%S</fc> | <fc=${config.colorBlue}>%a %Y-%m-%d</fc>" "date" 10
                                    , Run XMonadLog
                                    , Run Kbd [ ("us", "<fc=${config.colorWhite}>US</fc>")
                                                , ("ru", "<fc=${config.colorWhite}>RU</fc>")
                                                , ("de", "<fc=${config.colorWhite}>DE</fc>")
                                    ]
                       ]
                       , sepChar  = "%"
                       , alignSep = "}{"
                       , template = " %XMonadLog% }{ %kbd% | %date% | %alsa:default:Master% "
                       }
            '';
        };
    };

    config = {
        home.username = usrname;
        home.homeDirectory = "/home/"+usrname;

        home.sessionVariables = {
        };

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

            telegram-desktop
            discord
            flameshot
            obs-studio

            xmobar
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

        # xmonad setup
        xdg.configFile."xmonad/xmonad.hs".text = (builtins.readFile ./dotfiles/xmonad.hs) + ''
            -- Home-Manager settings

            setWallpaperCmd = spawn "feh --randomize --bg-fill $MEDIA/wallpapers/${config.wallpaperDir}"

            mySpace :: Integer
            mySpace = ${builtins.toString config.windowSpace}

            configDir :: String
            configDir = "/home/${usrname}/.config/xmonad/"

            myBgColor = "${config.bgColor}"
            myFgColor = "${config.fgColor}"
            myBarHeight = ${builtins.toString config.barheight}

            myLayout = ${config.xmonadLayouts}

            colorBlue = "${config.colorBlue}"
            colorWhite = "${config.colorWhite}"
            colorMagenta = "${config.colorMagenta}"
        '';
        home.file.".xmobarrc".text = config.xmobarOptions;

        # neofetch setup
        xdg.configFile."neofetch/config.conf".source = ./dotfiles/neofetch.conf;

        # zathura setup
        xdg.configFile."zathura/zathurarc".text = ''
            set window-title-basename "true"
            set selection-clipboard "clipboard"

            set notification-error-bg       "#ff5555" # Red
            set notification-error-fg       "${config.colorWhite}" # Foreground
            set notification-warning-bg     "#ffb86c" # Orange
            set notification-warning-fg     "#44475a" # Selection
            set notification-bg             "${config.bgColor}" # Background
            set notification-fg             "${config.colorWhite}" # Foreground
            set completion-bg               "${config.bgColor}" # Background
            set completion-fg               "#6272a4" # Comment
            set completion-group-bg         "${config.bgColor}" # Background
            set completion-group-fg         "#6272a4" # Comment
            set completion-highlight-bg     "#44475a" # Selection
            set completion-highlight-fg     "${config.colorWhite}" # Foreground
            set index-bg                    "${config.bgColor}" # Background
            set index-fg                    "${config.colorWhite}" # Foreground
            set index-active-bg             "#44475a" # Current Line
            set index-active-fg             "${config.colorWhite}" # Foreground
            set inputbar-bg                 "${config.bgColor}" # Background
            set inputbar-fg                 "${config.colorWhite}" # Foreground
            set statusbar-bg                "${config.bgColor}" # Background
            set statusbar-fg                "${config.colorWhite}" # Foreground
            set highlight-color             "#ffb86c" # Orange
            set highlight-active-color      "${config.colorMagenta}" # Pink
            set default-bg                  "${config.bgColor}" # Background
            set default-fg                  "${config.colorWhite}" # Foreground
            set render-loading              true
            set render-loading-fg           "${config.bgColor}" # Background
            set render-loading-bg           "${config.colorWhite}" # Foreground

            set recolor-lightcolor          "${config.bgColor}" # Background
            set recolor-darkcolor           "${config.colorWhite}" # Foreground

            set adjust-open width
            # set recolor true
            set guioptions none

            map <S-Up> feedkeys "zI"
            map <S-Down> feedkeys "zO"

            set zoom-max 50000

            set synctex true
            set synctex-editor-command "nvr --remote-silent +%{line} %{input}"
        '';

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
