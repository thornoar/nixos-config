{ lib, pkgs, inputs, system, config, ... }:

{
    config = 
    let dotfile = str: lib.path.append config.dotfiledir str;
    in 
    {
        dotfiledir = lib.mkForce ../dotfiles;
        # xmonad setup
        xdg.configFile."xmonad/xmonad.hs".text = (builtins.readFile (dotfile "xmonad.hs")) + ''
            -- Home-Manager settings

            setWallpaperCmd = spawn "xwallpaper --center $MEDIA/wallpapers/${config.wallpaperDir}/$(ls $MEDIA/wallpapers/${config.wallpaperDir} | shuf -n 1)"

            mySpace :: Integer
            mySpace = ${builtins.toString config.windowSpace}

            myBgColor = "${config.bgColor0}"
            myFgColor = "${config.fgColor}"
            myBarHeight = ${builtins.toString config.barheight}

            myLayout = ${config.xmonadLayouts}

            colorBlue0 = "${config.colorBlue0}"
            colorBlue2 = "${config.colorBlue2}"
            colorWhite = "${config.colorWhite0}"
            colorBlack = "${config.colorBlack}"
            colorLowWhite = "${config.colorWhite3}"
            colorMagenta0 = "${config.colorMagenta0}"
            colorYellow = "${config.colorYellow0}"
            colorRed = "${config.colorRed0}"
        '';

        # zathura setup
        xdg.configFile."zathura/zathurarc".text = ''
            set window-title-basename "true"
            set selection-clipboard "clipboard"

            set notification-error-bg       "${config.colorRed0}" # Red
            set notification-error-fg       "${config.colorWhite1}" # Foreground
            set notification-warning-bg     "${config.colorOrange}" # Orange
            set notification-warning-fg     "${config.bgColor1}" # Selection
            set notification-bg             "${config.bgColor0}" # Background
            set notification-fg             "${config.colorWhite1}" # Foreground
            set completion-bg               "${config.bgColor0}" # Background
            set completion-fg               "${config.colorBlue2}" # Comment
            set completion-group-bg         "${config.bgColor0}" # Background
            set completion-group-fg         "${config.colorBlue2}" # Comment
            set completion-highlight-bg     "${config.bgColor1}" # Selection
            set completion-highlight-fg     "${config.colorWhite1}" # Foreground
            set index-bg                    "${config.bgColor0}" # Background
            set index-fg                    "${config.colorWhite1}" # Foreground
            set index-active-bg             "${config.bgColor1}" # Current Line
            set index-active-fg             "${config.colorWhite1}" # Foreground
            set inputbar-bg                 "${config.bgColor0}" # Background
            set inputbar-fg                 "${config.colorWhite1}" # Foreground
            set statusbar-bg                "${config.bgColor0}" # Background
            set statusbar-fg                "${config.colorWhite1}" # Foreground
            set highlight-color             "${config.colorOrange}" # Orange
            set highlight-active-color      "${config.colorMagenta0}" # Pink
            set default-bg                  "${config.bgColor0}" # Background
            set default-fg                  "${config.colorWhite1}" # Foreground
            set render-loading              true
            set render-loading-fg           "${config.bgColor0}" # Background
            set render-loading-bg           "${config.colorWhite1}" # Foreground

            set recolor-lightcolor          "${config.bgColor0}" # Background
            set recolor-darkcolor           "${config.colorWhite1}" # Foreground

            set adjust-open width
            # set recolor true
            set guioptions none

            map <S-Up> feedkeys "zI"
            map <S-Down> feedkeys "zO"

            set zoom-max 50000

            set synctex true
            set synctex-editor-command "nvr --remote-silent +%{line} %{input}"
        '';

        programs.neovim = {
            enable = true;
        };
        xdg.configFile."nvim/ftdetect".source = dotfile "nvim/ftdetect";
        xdg.configFile."nvim/syntax".source = dotfile "nvim/syntax";
        xdg.configFile."nvim/UltiSnips".source = dotfile "nvim/UltiSnips";
        xdg.configFile."nvim/after".source = dotfile "nvim/after";
        xdg.configFile."nvim/init.lua".text = (builtins.readFile (dotfile "nvim/init.lua")) + ''
            require('lualine').setup{
                options = {
                    icons_enabled = true,
                    theme = {
                        normal = {
                            a = { fg = "${config.bgColor0}", bg = "${config.colorBlue0}", gui = 'bold' },
                            b = { fg = "${config.colorWhite3}", bg = "${config.bgColor1}" },
                            c = { fg = "${config.colorWhite3}", bg = "${config.bgColor1}" },
                        },
                        command = { a = { fg = "${config.bgColor0}", bg = "${config.colorYellow1}", gui = 'bold' } },
                        insert = { a = { fg = "${config.bgColor0}", bg = "${config.colorRed1}", gui = 'bold' } },
                        visual = { a = { fg = "${config.bgColor0}", bg = "${config.colorMagenta0}", gui = 'bold' } },
                        terminal = { a = { fg = "${config.bgColor0}", bg = "${config.colorCyan}", gui = 'bold' } },
                        replace = { a = { fg = "${config.bgColor0}", bg = "${config.colorBlue0}", gui = 'bold' } },
                        inactive = {
                            a = { fg = "${config.bgColor0}", bg = "${config.bgColor0}", gui = 'bold' },
                            b = { fg = "${config.bgColor0}", bg = "${config.bgColor0}" },
                            c = { fg = "${config.bgColor0}", bg = "${config.bgColor1}" },
                        },
                    },
                    component_separators = '|',
                    section_separators = "",
                },
                sections = {
                    lualine_a = {'mode'},
                    lualine_b = {'branch', 'diff', 'diagnostics'},
                    lualine_c = {'filename', keymap},
                    lualine_x = {'filetype'},
                    lualine_y = {'progress'},
                    lualine_z = {'location'},
                },
            }
        '';

        programs.alacritty = {
            enable = true;
            settings = {
                colors.bright = {
                    black = "${config.bgColor2}";
                    blue = "${config.colorBlue1}";
                    cyan = "${config.colorCyan}";
                    green = "${config.colorGreen1}";
                    magenta = "${config.colorMagenta1}";
                    red = "${config.colorRed1}";
                    white = "${config.colorWhite1}";
                    yellow = "${config.colorYellow1}";
                };
                colors.dim = {
                    black = "${config.bgColor0}";
                    blue = "${config.colorBlue1}";
                    cyan = "${config.colorCyan}";
                    green = "${config.colorGreen1}";
                    magenta = "${config.colorMagenta1}";
                    red = "${config.colorRed1}";
                    white = "${config.bgColor2}";
                    yellow = "${config.colorYellow1}";
                };
                colors.normal = {
                    black = "${config.bgColor0}";
                    blue = "${config.colorBlue1}";
                    cyan = "${config.colorCyan}";
                    green = "${config.colorGreen1}";
                    magenta = "${config.colorMagenta1}";
                    red = "${config.colorRed1}";
                    white = "${config.colorWhite0}";
                    yellow = "${config.colorYellow1}";
                };
                colors.primary = {
                    background = config.bgColor0;
                    bright_foreground = config.brfgColor;
                    foreground = config.fgColor;
                };
                cursor = {
                    style = "Underline";
                };
                env = {
                    COLORTERM = "truecolor";
                    TERM = "xterm-256color";
                };
                font = {
                    size = config.fontsize;
                };
                font.bold = {
                    family = config.font;
                    style = "Bold";
                };
                font.bold_italic = {
                    family = config.font;
                    style = "Bold Italic";
                };
                font.italic = {
                    family = config.font;
                    style = "Italic";
                };
                font.normal = {
                    family = config.font;
                    style = "Regular";
                };
                window.padding = {
                    x = config.padding.x;
                    y = config.padding.y;
                };
                window.opacity = config.windowOpacity;
                key_bindings = [
                    { key = "PageUp"; action = "ScrollLineUp"; }
                    { key = "PageDown"; action = "ScrollLineDown"; }
                    { key = "PageUp"; mods = "Alt"; action = "ScrollPageUp"; }
                    { key = "PageDown"; mods = "Alt"; action = "ScrollPageDown"; }
                ];
            };
        };

        xdg.configFile."broot/conf.hjson".source = dotfile "broot/conf.hjson";
        xdg.configFile."broot/verbs.hjson".source = dotfile "broot/verbs.hjson";
        xdg.configFile."broot/colorscheme.hjson".text = ''
            skin: {
                #default: #235219178 none / #189174147 none
                default: "${config.colorWhite3} None / ${config.colorWhite4} None"
                tree: "#707080 None / #606060 None"
                parent: "${config.colorYellow0} None / ${config.colorYellow1} None"
                file: "None None / None  None Italic"
                directory: "${config.colorBlue1} None Bold / ${config.colorBlue1} None"
                exe: "#b8bb26 None"
                link: "#689d6a None"
                pruning: "#7c6f64 None"
                perm__: "None None"
                # perm_r: "#d79921 None"
                # perm_w: "#cc241d None"
                # perm_x: "#98971a None"
                perm_r: "${config.colorYellow0} None"
                perm_w: "${config.colorRed1} None"
                perm_x: "${config.colorOrange} None"
                owner: "${config.colorBlue0} None Bold"
                group: "${config.colorMagenta1} None"
                count: "#458588 #32302f"
                dates: "#a89984 None"
                sparse: "#fabd2f None"
                content_extract: "ansi(29) None"
                content_match: ""${config.colorMagenta0} None Bold""
                git_branch: "#fbf1c7 None"
                git_insertions: "${config.colorGreen0} None"
                git_deletions: "#be0f17 None"
                git_status_current: "#3c3836 None"
                git_status_modified: "${config.colorGreen0} None"
                git_status_new: "#68bb26 None Bold"
                git_status_ignored: "#d5c4a1 None"
                git_status_conflicted: "#cc241d None"
                git_status_other: "#cc241d None"
                selected_line: "None #3c3836 / None #32302f"
                char_match: "${config.colorYellow1} None"
                file_error: "${config.colorRed1} None"
                flag_label: "#bdae93 None"
                flag_value: "#d3869b None Bold"
                input: "#fbf1c7 None / #bdae93 None"
                status_error: "#d5c4a1 #cc241d"
                status_job: "${config.colorYellow1} #3c3836"
                status_normal: "None #282625 / None None"
                status_italic: "#d3869b #282625 / None None"
                status_bold: "#d3869b #282625 Bold / None None"
                status_code: "#fbf1c7 #282625 / None None"
                status_ellipsis: "#fbf1c7 #282625  Bold / None None"
                purpose_normal: "None None"
                purpose_italic: "#b16286 None"
                purpose_bold: "#b16286 None Bold"
                purpose_ellipsis: "None None"
                scrollbar_track: "#504945 None / #32302f None"
                scrollbar_thumb: "#d5c4a1 None / #665c54 None"
                help_paragraph: "None None"
                help_bold: "#d65d0e None Bold"
                help_italic: "#d3869b None"
                help_code: "#8ec07c #32302f"
                help_headers: "#fe8019 None Bold"
                help_table_border: "#504945 None"
                preview_title: "#ebdbb2 #282828 / #bdae93 #282828"
                preview: "#ebdbb2 #282828 / #ebdbb2 #282828"
                preview_line_number: "#7c6f64 None / #7c6f64 #282828"
                preview_match: "None ansi(29) Bold"
                hex_null: "#bdae93 None"
                hex_ascii_graphic: "#d5c4a1 None"
                hex_ascii_whitespace: "#98971a None"
                hex_ascii_other: "#fe8019 None"
                hex_non_ascii: "#d65d0e None"
                staging_area_title: "#ebdbb2 #282828 / #bdae93 #282828"
                mode_command_mark: "gray(5) ansi(204) Bold"
                good_to_bad_0: "ansi(28)"
                good_to_bad_1: "ansi(29)"
                good_to_bad_2: "ansi(29)"
                good_to_bad_3: "ansi(29)"
                good_to_bad_4: "ansi(29)"
                good_to_bad_5: "ansi(100)"
                good_to_bad_6: "ansi(136)"
                good_to_bad_7: "ansi(172)"
                good_to_bad_8: "ansi(166)"
                good_to_bad_9: "ansi(196)"
            }
        '';

        # asymptote setup
        home.file.".asy/config.asy".source = dotfile "config.asy";

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
                        background-color: ${config.colorBlue1} !important;
                        color: ${config.colorWhite2} !important;
                        }
                    '';
                };
            };
        };

        # R setup
        home.file.".Rprofile".source = dotfile "Rprofile";

        # xmobar setup
        xdg.configFile."xmobar/xmobarrc".text = config.xmobarOptions;

        # neofetch setup
        xdg.configFile."neofetch/config.conf".source = dotfile "neofetch.conf";

        # keynav setup
        services.keynav.enable = true;
        xdg.configFile."keynav/keynavrc".source = dotfile "keynavrc";
    };
}
