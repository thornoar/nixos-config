{ lib, pkgs, config, ... }:

{
    config = 
    let
        dotfile = str: lib.path.append ../dotfiles str;
        # project = str: lib.path.append /home/ramak/projects str;
        ts = builtins.toString;
    in 
    {
        # xmonad setup
        xdg.configFile."xmonad/xmonad.hs".text = (builtins.readFile (dotfile "xmonad.hs")) + (
        let
            cmd = if (builtins.pathExists (lib.path.append /home/ramak/media/wallpapers config.wallpaperDir))
                  then "hsetroot -cover $MEDIA/wallpapers/${config.wallpaperDir}/$(ls $MEDIA/wallpapers/${config.wallpaperDir} | shuf -n 1) -gamma ${ts config.wallpaperGamma} -contrast ${ts config.wallpaperContrast}"
                  else "echo 'no wallpapers'";
        in ''

            -- Home-Manager settings

            setWallpaperCmd = spawn "${cmd}"

            myBorderWidth :: Dimension
            myBorderWidth = ${ts config.windowBorderWidth}

            mySpace :: Int
            mySpace = ${ts config.windowSpace} `div` 2

            myBgColor = "${config.bgColor0}"
            myFgColor = "${config.fgColor}"
            myBarHeight = ${ts config.barHeight}
            myMagnifiedScale = ${ts config.magnifiedScale}

            myFloatingRectangle :: W.RationalRect
            myFloatingRectangle = W.RationalRect ((1 - ${config.scratchpadWidth}) / 2) ((1 - ${config.scratchpadHeight}) / 2) (${config.scratchpadWidth}) (${config.scratchpadHeight}) 

            colorMagenta1 = "${config.colorMagenta1}"
            colorBlue2 = "${config.colorBlue2}"
            colorWhite = "${config.colorWhite0}"
            colorBlack = "${config.colorBlack}"
            colorLowWhite = "${config.colorWhite3}"
            colorMagenta0 = "${config.colorMagenta0}"
            colorYellow = "${config.colorYellow0}"
            colorRed = "${config.colorRed0}"
        '');

        # zathura setup
        xdg.configFile."zathura/zathurarc".text = ''
            set window-title-basename "true"
            set selection-clipboard "clipboard"

            set notification-error-bg       "${config.colorRed0}" # Red
            set notification-error-fg       "${config.colorWhite1}" # Foreground
            set notification-warning-bg     "${config.colorOrange0}" # Orange
            set notification-warning-fg     "${config.bgColor2}" # Selection
            set notification-bg             "${config.bgColor0}" # Background
            set notification-fg             "${config.colorWhite1}" # Foreground
            set completion-bg               "${config.bgColor0}" # Background
            set completion-fg               "${config.colorBlue2}" # Comment
            set completion-group-bg         "${config.bgColor0}" # Background
            set completion-group-fg         "${config.colorBlue2}" # Comment
            set completion-highlight-bg     "${config.bgColor2}" # Selection
            set completion-highlight-fg     "${config.colorWhite1}" # Foreground
            set index-bg                    "${config.bgColor0}" # Background
            set index-fg                    "${config.colorWhite1}" # Foreground
            set index-active-bg             "${config.bgColor2}" # Current Line
            set index-active-fg             "${config.colorWhite1}" # Foreground
            set inputbar-bg                 "${config.bgColor0}" # Background
            set inputbar-fg                 "${config.colorWhite1}" # Foreground
            set statusbar-bg                "${config.bgColor0}" # Background
            set statusbar-fg                "${config.colorWhite1}" # Foreground
            set highlight-color             "${config.colorOrange0}" # Orange
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
            map <M-Up> feedkeys "<PageUp>"
            map <M-Down> feedkeys "<PageDown>"

            set zoom-max 50000

            set synctex true
            set synctex-editor-command "nvr --remote-silent +%{line} %{input}"
        '';

        xdg.configFile."nvim/ftdetect".source = dotfile "nvim/ftdetect";
        xdg.configFile."nvim/syntax".source = dotfile "nvim/syntax";
        xdg.configFile."nvim/UltiSnips".source = dotfile "nvim/UltiSnips";
        xdg.configFile."nvim/after".source = dotfile "nvim/after";
        xdg.configFile."nvim/init.lua".text = (builtins.readFile (dotfile "nvim/init.lua")) + ''

            require('tabby.tabline').set(function(line)
                return {
                    line.tabs().foreach(function(tab)
                        local hl = tab.is_current() and { fg = "${config.colorMagenta0}", bg = "${config.bgColor0}" } or { fg = "${config.colorWhite3}", bg = "${config.bgColor0}" }
                        return {
                            line.sep("", hl, { bg = "${config.bgColor0}" }),
                            tab.name(),
                            line.sep("", hl, { bg = "${config.bgColor0}" }),
                            hl = hl,
                            margin = ' ',
                        }
                    end),
                    hl = { bg = "${config.bgColor0}" },
                }
            end)

            require('lualine').setup{
                options = {
                    icons_enabled = true,
                    theme = {
                        normal = {
                            a = { fg = "${config.bgColor0}", bg = "${config.colorMagenta1}", gui = 'bold' },
                            b = { fg = "${config.colorWhite3}", bg = "${config.bgColor0}" },
                            c = { fg = "${config.colorWhite3}", bg = "${config.bgColor0}" },
                        },
                        command = { a = { fg = "${config.bgColor0}", bg = "${config.colorYellow1}", gui = 'bold' } },
                        insert = { a = { fg = "${config.bgColor0}", bg = "${config.colorRed1}", gui = 'bold' } },
                        visual = { a = { fg = "${config.bgColor0}", bg = "${config.colorMagenta0}", gui = 'bold' } },
                        terminal = { a = { fg = "${config.bgColor0}", bg = "${config.colorCyan}", gui = 'bold' } },
                        replace = { a = { fg = "${config.bgColor0}", bg = "${config.colorMagenta1}", gui = 'bold' } },
                        inactive = {
                            a = { fg = "${config.bgColor0}", bg = "${config.bgColor0}", gui = 'bold' },
                            b = { fg = "${config.bgColor0}", bg = "${config.bgColor0}" },
                            c = { fg = "${config.bgColor0}", bg = "${config.bgColor0}" },
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

        xdg.configFile."broot/conf.hjson".source = dotfile "broot/conf.hjson";
        xdg.configFile."broot/verbs.hjson".source = dotfile "broot/verbs.hjson";
        xdg.configFile."broot/colorscheme.hjson".text = ''
            skin: {
                default: "${config.colorWhite3} None / ${config.colorWhite4} None"
                tree: "${config.bgColor2} None / ${config.bgColor1} None"
                parent: "${config.colorYellow0} None / ${config.colorYellow1} None"
                file: "None None / None  None Italic"
                directory: "${config.colorBlue1} None Bold / ${config.colorBlue1} None"
                exe: "${config.colorGreen1} None"
                link: "${config.colorGreen1} None"
                pruning: "${config.colorWhite4} None"

                perm__: "None None"
                perm_r: "${config.colorYellow0} None"
                perm_w: "${config.colorRed1} None"
                perm_x: "${config.colorOrange0} None"

                owner: "${config.colorWhite2} None"
                group: "${config.colorWhite3} None"
                count: "${config.colorWhite4} None"
                size: "${config.colorWhite4} None"
                dates: "${config.colorWhite3} None"
                sparse: "${config.colorYellow0} None"

                content_extract: "ansi(29) None"
                content_match: "${config.colorMagenta0} None Bold"

                git_branch: "${config.colorWhite2} None"
                git_insertions: "${config.colorGreen4} None"
                git_deletions: "${config.colorRed1} None"
                git_status_current: "${config.bgColor2} None"
                git_status_modified: "${config.colorBlue1} None"
                git_status_new: "${config.colorGreen4} None Bold"
                git_status_ignored: "${config.colorWhite3} None"
                git_status_conflicted: "${config.colorRed1} None"
                git_status_other: "${config.colorRed1} None"

                selected_line: "None ${config.bgColor1} / None ${config.bgColor1}"

                char_match: "${config.colorMagenta3} None"
                # char_match: "${config.colorOrange1} None"

                file_error: "${config.colorRed1} None"
                flag_label: "${config.colorWhite4} None"
                flag_value: "${config.colorMagenta3} None Bold"

                input: "${config.colorWhite2} None / ${config.colorWhite4} None"

                status_error: "${config.colorWhite3} ${config.colorRed0}"
                status_job: "${config.colorYellow1} ${config.bgColor1}"
                status_normal: "None ${config.bgColor0} / None None"
                status_italic: "${config.colorMagenta3} ${config.bgColor0} / None None"
                status_bold: "${config.colorMagenta3} ${config.bgColor0} Bold / None None"
                status_code: "${config.colorWhite2} ${config.bgColor0} / None None"
                status_ellipsis: "${config.colorWhite2} ${config.bgColor0} Bold / None None"

                purpose_normal: "None None"
                purpose_italic: "${config.colorMagenta3} None"
                purpose_bold: "${config.colorMagenta3} None Bold"
                purpose_ellipsis: "None None"

                scrollbar_track: "${config.bgColor0} None / ${config.bgColor0} None"
                scrollbar_thumb: "${config.colorWhite3} None / ${config.colorWhite4} None"

                help_paragraph: "None None"
                help_bold: "${config.colorOrange1} None Bold"
                help_italic: "${config.colorMagenta3} None"
                help_code: "${config.colorGreen4} ${config.bgColor1}"
                help_headers: "${config.colorOrange1} None Bold"
                help_table_border: "${config.bgColor0} None"

                preview_title: "${config.colorWhite3} None / ${config.colorWhite4} None"
                preview: "${config.colorWhite3} ${config.bgColor0} / ${config.colorWhite3} ${config.bgColor0}"
                preview_line_number: "${config.colorWhite4} None / ${config.colorWhite4} None"
                preview_match: "None ${config.bgColor0} Bold"

                hex_null: "${config.colorWhite4} None"
                hex_ascii_graphic: "${config.colorWhite3} None"
                hex_ascii_whitespace: "${config.colorOrange0} None"
                hex_ascii_other: "${config.colorOrange1} None"
                hex_non_ascii: "${config.colorOrange1} None"

                staging_area_title: "${config.colorWhite3} None / ${config.colorWhite4} None"

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
            ext_colors: {
                txt: "${config.colorWhite3}"
                asy: "${config.colorYellow1}"
                py: "${config.colorCyan}"
                hs: "${config.colorMagenta2}"
                vim: "${config.colorGreen2}"
                nix: "${config.colorMagenta1}"
                rs: "${config.colorOrange1}"
                lua: "${config.colorBlue0}"
                pdf: "${config.colorRed2}"
                epub: "${config.colorRed2}"
                djvu: "${config.colorRed2}"
                # lock: "${config.colorWhite0}"
                # torrent: "${config.colorGreen1}"
            }
        '';

        # asymptote setup
        home.file.".asy/config.asy".source = dotfile "config.asy";

        # moc setup
        home.file.".moc/config".text = ''
            Theme = nightly_theme
            Keymap = keymap
            Repeat = yes
            Shuffle = no
            AutoNext = yes
        '';
        home.file.".moc/keymap".text = ''
            go    = ENTER RIGHT
            go_up = U LEFT
            #seek_forward  = RIGHT
            #seek_backward = LEFT
        '';

        # mpv setup
        xdg.configFile."mpv/mpv.conf".text = ''
            loop-file=inf
        '';

        # programs
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
                    x = config.terminalPadding.x;
                    y = config.terminalPadding.y;
                };
                window.opacity = config.terminalOpacity;
                key_bindings = [
                    { key = "PageUp"; action = "ScrollLineUp"; }
                    { key = "PageDown"; action = "ScrollLineDown"; }
                    { key = "PageUp"; mods = "Alt"; action = "ScrollPageUp"; }
                    { key = "PageDown"; mods = "Alt"; action = "ScrollPageDown"; }
                ];
            };
        };
        programs.firefox.profiles.default = {
            bookmarks = [
                {
                    name = "yt | YouTube";
                    url = "https://youtube.com";
                    keyword = "yt";
                }
                {
                    name = "st | Syncthing";
                    url = "http://127.0.0.1:8384/";
                    keyword = "st";
                }
                {
                    name = "ym | YouTube Music";
                    url = "https://music.youtube.com";
                    keyword = "ym";
                }
                {
                    name = "gh | GitHub";
                    url = "https://github.com/thornoar?tab=repositories";
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
                {
                    name = "lw | lesswrong.com";
                    url = "https://www.lesswrong.com/";
                    keyword = "lw";
                }
                {
                    name = "see | english.stackexchange.com";
                    url = "https://english.stackexchange.com/";
                    keyword = "see";
                }
                {
                    name = "sej | japanese.stackexchange.com";
                    url = "https://japanese.stackexchange.com/";
                    keyword = "sej";
                }
                {
                    name = "su | superuser.com";
                    url = "https://superuser.com/";
                    keyword = "su";
                }
                {
                    name = "sef | money.stackexchange.com";
                    url = "https://money.stackexchange.com/";
                    keyword = "sef";
                }
                {
                    name = "ses | software.stackexchange.com";
                    url = "https://softwareengineering.stackexchange.com/";
                    keyword = "ses";
                }
                {
                    name = "col | numbeo.com/cost-of-living";
                    url = "https://www.numbeo.com/cost-of-living/";
                    keyword = "col";
                }
                {
                    name = "sd | sciencedaily.com";
                    url = "https://www.sciencedaily.com/";
                    keyword = "sd";
                }
            ];
            userChrome = ''
                #unified-extensions-button, #unified-extensions-button > .toolbarbutton-icon {
                    width: 0px !important;
                    padding: 0px !important;
                }

                .titlebar-buttonbox-container{ display:none }

                #back-button, #forward-button { display:none!important; }

                #PanelUI-button { display:none!important; }

                #alltabs-button {
                    display: none !important;
                }

                #navigator-toolbox { font-family:Hack !important }

                #urlbar ::-moz-selection,
                .searchbar-textbox ::-moz-selection {
                    background-color: ${config.colorBlue1} !important;
                    color: ${config.colorWhite2} !important;
                }
            '';
        };
        programs.zsh.initExtra = ''
            autoload -U colors && colors
            PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "

            function preexec() {
                timer=$(($(date +%s%0N)/1000000))
            }

            function precmd() {
                echo -ne '\e[4 q'
                if [ $timer ]; then
                now=$(($(date +%s%0N)/1000000))
                elapsed=$(($now-$timer))
                export RPROMPT="< %{$fg[yellow]%}''${elapsed}ms%{$reset_color%}"
                unset timer
                fi
            }

            source $NIXOS_CONFIG/dotfiles/br.sh

            bindkey "^[[1;3D" backward-word 
            bindkey "^[[1;3C" forward-word

            typeset -U PATH path
            BINPATH="$PROJECTS"
            path+=("$BINPATH" "''${BINPATH}"/*/bin)
            export PATH

            eval "$(fzf --zsh)"
            bindkey "^[[1;5B" fzf-file-widget
            bindkey "^[[1;5C" fzf-cd-widget
            bindkey "^[[1;5A" fzf-history-widget
            export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
            export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
            _fzf_comprun() {
                local command=$1
                shift
                # ((((
                case "$command" in
                    cd) fzf --preview "eza --tree --color=always {} | head -200" "$@" ;;
                    export|unset) fzf --preview "eval 'echo \$'{}" "$@" ;;
                    ssh) fzf --preview "dig {}" "$@" ;;
                    *) fzf --preview "--preview 'bat -n --color=always --line-range :500 {}'" "$@" ;;
                esac
            }

            TIMEFMT=$'\n'\
            'time:          %U user %S system %P cpu %*E total'$'\n'\
            'max memory:    %M '$MAX_MEMORY_UNITS'MB'
        '';

        home.file.".Rprofile".source = dotfile "Rprofile";

        # xmobar setup
        xdg.configFile."xmobar/xmobarrc".text = config.xmobarOptions;

        # neofetch setup
        xdg.configFile."neofetch/config.conf".source = dotfile "neofetch.conf";

        # mimeapps handling
        xdg.configFile."mimeapps.list".force = true;
        # Inkview desktop file
        home.file.".local/share/applications/inkview.desktop".text = ''
            [Desktop Entry]
            Type=Application
            Name=Inkview
            Comment=View SVG files
            Exec=inkview %U
            Categories=Graphics;2DGraphics;
            MimeType=image/svg+xml;
        '';

        # typst libraries enabling

        xdg.dataFile = 
        let 
            path = /home/ramak/projects/typst-libraries;
        in if (builtins.pathExists path) then builtins.listToAttrs (
            lib.lists.flatten (
                lib.lists.forEach
                (lib.filesystem.listFilesRecursive path)
                (filename:
                    let
                        strname = ts filename;
                        last = lib.lists.last (lib.strings.splitString "/" strname);
                        base = lib.lists.head (lib.strings.splitString "." last);
                    in
                    [
                        {
                            name = "typst/packages/local/" + base + "/0.0.0/main.typ";
                            value = { source = strname; };
                        }
                        {
                            name = "typst/packages/local/" + base + "/0.0.0/typst.toml";
                            value = { text = ''
                                [package]
                                name = "${base}"
                                version = "0.0.0"
                                entrypoint = "main.typ"
                                authors = ["Roman Maksimovich"]
                            ''; };
                        }
                    ]
                )
            )
        ) else {};

        home.file.".local/bin/pshash" =
        let path = /home/ramak/projects/pshash/bin/pshash.nixos.x86_64;
        in if (builtins.pathExists path) then { source = path; force = true; } else { text = "echo 'sorry, no pshash'"; };

        # keynav setup
        services.keynav.enable = true;
        xdg.configFile."keynav/keynavrc".source = dotfile "keynavrc";
    };
}
