{ lib, pkgs, config, ... }:
{
    options = 
    let
        opt = lib.mkOption;
        tp = lib.types;
        mkOpt = typ: df: opt {
            type = tp.${typ};
            default = df;
        };
    in
    {
        fontsize = opt { type = tp.int; };
        fontsizeBar = opt { type = tp.int; };
        wallpaperDir = mkOpt "str" "Landscapes";
        windowSpace = mkOpt "int" 5;

        bgColor0 = mkOpt "str" "#0b1012";#"#1e2127";
        bgColor1 = mkOpt "str" config.bgColor0;#"#2c3037";
        bgColor2 = mkOpt "str" "#43565c";
        fgColor = mkOpt "str" "#17a88b";
        brfgColor = mkOpt "str" "#00bc96";

        colorBlack = mkOpt "str" "#000000";

        colorWhite0 = mkOpt "str" "#ffffff";
        colorWhite1 = mkOpt "str" "#f8f8f2";
        colorWhite2 = mkOpt "str" "#e6efff";
        colorWhite3 = mkOpt "str" "#bbbbbb";

        colorYellow0 = mkOpt "str" "#f1fa8c";
        colorYellow1 = mkOpt "str" "#d19a66";
        colorRed0 = mkOpt "str" "#ff5555";
        colorRed1 = mkOpt "str" "#e06c75";
        colorOrange = mkOpt "str" "#ffb86c";

        colorBlue0 = mkOpt "str" "#bd93f9";
        colorBlue2 = mkOpt "str" "#6272a4";
        colorBlue1 = mkOpt "str" "#61afef";
        colorCyan = mkOpt "str" "#56b6c2";
        colorGreen0 = mkOpt "str" "#89a870";
        colorGreen1 = mkOpt "str" "#329c48";

        colorMagenta0 = mkOpt "str" "#ff79c6";
        colorMagenta1 = mkOpt "str" "#c678dd";

        font = mkOpt "str" "Hack";
        padding = mkOpt "attrs" { x = 6; y = 6; };
        barheight = mkOpt "int" 35;
        xmonadLayouts = mkOpt "str" "tall ||| Full ||| tabs ||| magnified ||| spirals";
        xmobarOptions = opt {
            type = tp.str;
            default = ''
                Config { font     = "xft:${config.font} Nerd Font Mono-${builtins.toString config.fontsizeBar}"
                       , bgColor0  = "${config.bgColor0}"
                       , fgColor  = "${config.fgColor}"
                       , position = TopH ${builtins.toString config.barheight}
                       , persistent = False
                       , hideOnStart = False
                       , allDesktops = True
                       , lowerOnStart = True
                       , commands = [
                                    Run Alsa "default" "Master"
                                        [ "--template", "<fc=${config.colorWhite1}><volumestatus></fc>"
                                        , "--suffix"  , "True"
                                        , "--"
                                        , "--on", ""
                                    ]
                                    , Run Date "<fc=${config.colorMagenta0}>%H:%M:%S</fc> | <fc=${config.colorBlue0}>%a %Y-%m-%d</fc>" "date" 10
                                    , Run XMonadLog
                                    , Run Kbd [ ("us", "<fc=${config.colorWhite1}>US</fc>")
                                                , ("ru", "<fc=${config.colorWhite1}>RU</fc>")
                                                , ("de", "<fc=${config.colorWhite1}>DE</fc>")
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
        # xmonad setup
        xdg.configFile."xmonad/xmonad.hs".text = (builtins.readFile ../dotfiles/xmonad.hs) + ''
            -- Home-Manager settings

            setWallpaperCmd = spawn "feh --randomize --bg-fill $MEDIA/wallpapers/${config.wallpaperDir}"

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
            viAlias = true;
            vimAlias = true;
            vimdiffAlias = true;
        };
        xdg.configFile."nvim/ftdetect".source = ../dotfiles/nvim/ftdetect;
        xdg.configFile."nvim/syntax".source = ../dotfiles/nvim/syntax;
        xdg.configFile."nvim/UltiSnips".source = ../dotfiles/nvim/UltiSnips;
        xdg.configFile."nvim/after".source = ../dotfiles/nvim/after;
        xdg.configFile."nvim/init.lua".text = (builtins.readFile ../dotfiles/nvim/init.lua) + ''
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
            local theme = {
                fill = 'TabLineFill',
                head = 'TabLine',
                current_tab = { fg = "${config.colorBlack}", bg = "${config.colorBlue0}", style = 'italic' },--'TabLineSel',
                tab = 'TabLine',
                win = 'TabLine',
                tail = 'TabLine',
            }
            require('tabby.tabline').set(function(line)
                return {
                    line.tabs().foreach(function(tab)
                        local hl = tab.is_current() and theme.current_tab or theme.tab
                        return {
                            line.sep("", hl, theme.fill),
                            tab.name(),
                            line.sep("", hl, theme.fill),
                            hl = hl,
                            margin = ' ',
                        }
                    end),
                }
            end)
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
                keyboard.bindings = [
                    { key = "PageUp"; action = "ScrollLineUp"; }
                    { key = "PageDown"; action = "ScrollLineDown"; }
                    { key = "PageUp"; mods = "Alt"; action = "ScrollPageUp"; }
                    { key = "PageDown"; mods = "Alt"; action = "ScrollPageDown"; }
                ];
            };
        };
    };
}
