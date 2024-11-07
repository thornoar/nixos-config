{ lib, config, pkgs, ... }:

{
    config = 
    let
        dotfile = str: lib.path.append ../src str;
        ts = builtins.toString;
        toLua = lib.attrsets.foldlAttrs (str: k: v: str + "M.${k} = \"${ts v}\"\n");
        toHaskell = mkstr: lib.attrsets.foldlAttrs (str: k: v: str + "${k} = ${if mkstr then "\"" + ts v + "\"" else ts v}\n");
        toCSS = mkstr: lib.attrsets.foldlAttrs (str: k: v: str + "@define-color ${k} ${if mkstr then "\"" + ts v + "\"" else ts v};\n");
        toConf = mkstr: lib.attrsets.foldlAttrs (str: k: v: str + "\$${k} = ${if mkstr then "\"" + ts v + "\"" else ts v}\n");
    in 
    {
        xdg.configFile."colors.css".text = (toCSS false "" config.colors);

        # Hyprland setup
        xdg.configFile."hypr/imports.conf".text = toConf false "" config.hyprland;
        # + ''
        #     $wallpaperCmd = swww img $(find $MEDIA/wallpapers/${config.wallpaper.dir} -type f | shuf -n 1) --transition-duration 1 --transition-type right
        # '';
        xdg.configFile."hypr/hyprpaper.conf".source = dotfile "hypr/hyprpaper.conf";
        xdg.configFile."hypr/hyprland.conf".source = dotfile "hypr/hyprland.conf";

        # Wofi setup
        xdg.configFile."wofi/size.css".text = ''
            * {
                font-size: ${ts config.hyprland.fontsize}pt;
                border-radius: ${ts config.hyprland.rounding}px;
            }
        '';
        xdg.configFile."wofi/style.css".source = dotfile "wofi/style.css";

        # Waybar setup
        xdg.configFile."waybar/size.css".text = ''
            * {
                font-size: ${ts config.hyprland.fontsizeWaybar}pt;
                border-radius: ${ts config.hyprland.rounding}px;
            }
        '';
        xdg.configFile."waybar/style.css".source = dotfile "waybar/style.css";
        xdg.configFile."waybar/config".text = ''
            {
                "layer": "top",
                "margin-top": ${ts config.hyprland.windowSpaceOuter},
                "margin-bottom": 0,
                "margin-left": ${ts config.hyprland.windowSpaceOuter},
                "margin-right": ${ts config.hyprland.windowSpaceOuter},
                "layer": "top",
                "spacing": 0,

                "include": [
                    "~/.config/waybar/modules.json"
                ],

                "modules-left": [
                    "hyprland/workspaces",
                ],

                "modules-center": [
                    "pulseaudio",
                    "clock",
                    "backlight",
                ],

                "modules-right": [
                    "hyprland/language",
                    "network",
                    "cpu",
                    "memory",
                    "battery", 
                ]
            }
        '';
        xdg.configFile."waybar/modules.json".source = dotfile "waybar/modules.json";
        
        # xmonad setup

        xdg.configFile."xmonad/lib/Colors.hs".text = toHaskell true ''
            module Colors where
        '' config.colors;
        xdg.configFile."xmonad/lib/Size.hs".text = toHaskell false ''
            module Size where
            import XMonad
            import Data.Ratio
            barHeight :: Dimension
            magnifiedScale :: Rational
            windowSpaceInner :: Int
            windowSpaceOuter :: Int
            windowBorderWidth :: Dimension
        '' config.xmonad;
        xdg.configFile."xmonad/lib/Misc.hs".text =
        let
            cmd = if (builtins.pathExists (lib.path.append /home/ramak/media/wallpapers config.wallpaper.dir))
                  then "hsetroot -cover $MEDIA/wallpapers/${config.wallpaper.dir}/$(ls $MEDIA/wallpapers/${config.wallpaper.dir} | shuf -n 1) -gamma ${ts config.wallpaper.gamma} -contrast ${ts config.wallpaper.contrast}"
                  else "echo 'no wallpapers'";
        in ''
            module Misc where
            setWallpaperCmd = "${cmd}"
        '';
        xdg.configFile."xmonad/xmonad.hs".source = dotfile "xmonad.hs";

        # Neovim configuration

        xdg.configFile."nvim/ftdetect".source = dotfile "nvim/ftdetect";
        xdg.configFile."nvim/syntax".source = dotfile "nvim/syntax";
        xdg.configFile."nvim/UltiSnips".source = dotfile "nvim/UltiSnips";
        xdg.configFile."nvim/after".source = dotfile "nvim/after";
        xdg.configFile."nvim/lua/colors.lua".text = toLua ''
            local M = {}
        '' config.colors + ''
            return M
        '';
        xdg.configFile."nvim/init.lua".source = dotfile "nvim/init.lua";

        # asymptote configuration
        home.file.".asy/config.asy".source = dotfile "config.asy";

        # moc configuration
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

        # mpv configuration
        xdg.configFile."mpv/mpv.conf".text = ''
            loop-file=inf
        '';

        # programs
        programs.alacritty = {
            enable = true;
            settings = {
                colors.bright = {
                    black = config.colors.bgColor3;
                    blue = config.colors.colorBlue1;
                    cyan = config.colors.colorCyan;
                    green = config.colors.colorGreen1;
                    magenta = config.colors.colorMagenta1;
                    red = config.colors.colorRed1;
                    white = config.colors.colorWhite1;
                    yellow = config.colors.colorYellow1;
                };
                colors.dim = {
                    black = config.colors.bgColor0;
                    blue = config.colors.colorBlue1;
                    cyan = config.colors.colorCyan;
                    green = config.colors.colorGreen1;
                    magenta = config.colors.colorMagenta1;
                    red = config.colors.colorRed1;
                    white = config.colors.bgColor3;
                    yellow = config.colors.colorYellow1;
                };
                colors.normal = {
                    black = config.colors.bgColor0;
                    blue = config.colors.colorBlue1;
                    cyan = config.colors.colorCyan;
                    green = config.colors.colorGreen1;
                    magenta = config.colors.colorMagenta1;
                    red = config.colors.colorRed1;
                    white = config.colors.colorWhite0;
                    yellow = config.colors.colorYellow1;
                };
                colors.primary = {
                    background = config.colors.bgColor0;
                    bright_foreground = config.colors.brfgColor;
                    foreground = config.colors.fgColor0;
                };
                cursor = {
                    style = "Underline";
                };
                env = {
                    COLORTERM = "truecolor";
                    TERM = "xterm-256color";
                };
                font = {
                    size = config.xmonad.fontsize;
                };
                font.bold = {
                    family = config.misc.systemFont;
                    style = "Bold";
                };
                font.bold_italic = {
                    family = config.misc.systemFont;
                    style = "Bold Italic";
                };
                font.italic = {
                    family = config.misc.systemFont;
                    style = "Italic";
                };
                font.normal = {
                    family = config.misc.systemFont;
                    style = "Regular";
                };
                window.padding = {
                    x = config.xmonad.terminalPaddingX;
                    y = config.xmonad.terminalPaddingY;
                };
                window.opacity = config.xmonad.terminalOpacity;
                keyboard.bindings = [
                    { key = "PageUp"; action = "ScrollLineUp"; }
                    { key = "PageDown"; action = "ScrollLineDown"; }
                    { key = "PageUp"; mods = "Alt"; action = "ScrollPageUp"; }
                    { key = "PageDown"; mods = "Alt"; action = "ScrollPageDown"; }
                ];
            };
        };

        # Kitty setup
        xdg.configFile."kitty/minimal.lua".source = dotfile "kitty/minimal.lua";
        programs.kitty = {
            enable = true;
            font = {
                size = config.hyprland.fontsize;
                name = "Hack";
            };
            shellIntegration = {
                mode = "no-cursor";
                enableZshIntegration = true;
            };
            settings = {
                cursor = config.colors.fgColor0;
                cursor_shape = "underline";
                cursor_underline_thickness = "1.0";
                cursor_beam_thickness = "1.0";
                window_padding_width = ts config.hyprland.terminalPadding;
                foreground = config.colors.fgColor0;
                background = config.colors.bgColor0;
                background_opacity = ts config.hyprland.terminalOpacity;
                color0 = config.colors.bgColor0;
                color8 = config.colors.bgColor3;
                color1 = config.colors.colorRed1;
                color9 = config.colors.colorRed1;
                color2 = config.colors.colorGreen1;
                color10 = config.colors.colorGreen1;
                color3 = config.colors.colorYellow1;
                color11 = config.colors.colorYellow1;
                color4 = config.colors.colorBlue1;
                color12 = config.colors.colorBlue1;
                color5 = config.colors.colorMagenta1;
                color13 = config.colors.colorMagenta1;
                color6 = config.colors.colorCyan;
                color14 = config.colors.colorCyan;
                color7 = config.colors.colorWhite1;
                color15 = config.colors.bgColor3;
                cursor_blink_interval = "0";
                confirm_os_window_close = "0";
                scrollback_pager = "";
                allow_remote_control = "yes";
                # listen_on = "unix:/tmp/kitty";
                mouse_hide_wait = "0.1";
            };
            extraConfig = ''
                map ctrl+shift+right
                map ctrl+shift+left
                map ctrl+shift+c copy_to_clipboard
                map ctrl+shift+v paste_from_clipboard
                map page_up scroll_line_up
                map page_down scroll_line_down
                map alt+page_up scroll_page_up
                map alt+page_down scroll_page_down
                map alt+end scroll_end
                map ctrl+shift+space show_scrollback

                # scrollback_pager nvim --noplugin -u ~/.config/kitty/minimal.lua -c "silent write! /tmp/kitty_scrollback_buffer | te cat /tmp/kitty_scrollback_buffer -"
                scrollback_pager nvim --noplugin -u ~/.config/kitty/minimal.lua -c "START"

                # action_alias kitty_scrollback_nvim kitten /home/ramak/.local/share/nvim/lazy/kitty-scrollback.nvim/python/kitty_scrollback_nvim.py
                # map kitty_mod+h kitty_scrollback_nvim
                # map kitty_mod+g kitty_scrollback_nvim --config ksb_builtin_last_cmd_output
                # mouse_map ctrl+shift+right press ungrabbed combine : mouse_select_command_output : kitty_scrollback_nvim --config ksb_builtin_last_visited_cmd_output
            '';
        };
        programs.firefox.profiles.default = {
            bookmarks = [
                { name = "yt | YouTube"; url = "https://youtube.com"; keyword = "yt"; }
                { name = "st | Syncthing"; url = "http://127.0.0.1:8384/"; keyword = "st"; }
                { name = "ym | YouTube Music"; url = "https://music.youtube.com"; keyword = "ym"; }
                { name = "gh | GitHub"; url = "https://github.com/thornoar?tab=repositories"; keyword = "gh"; }
                { name = "qw | Work Mail"; url = "https://mail.google.com/mail/u/1/#inbox"; keyword = "qw"; }
                { name = "as | Personal Mail"; url = "https://mail.google.com/mail/u/0/#inbox"; keyword = "as"; }
                { name = "mn | MyNixOS"; url = "https://mynixos.com"; keyword = "mn"; }
                { name = "tr | RuTracker"; url = "https://rutracker.org"; keyword = "tr"; }
                { name = "li | LibGen"; url = "https://libgen.is"; keyword = "li"; }
                { name = "ni | Nixhub.io"; url = "https://www.nixhub.io/"; keyword = "ni"; }
                { name = "lw | lesswrong.com"; url = "https://www.lesswrong.com/"; keyword = "lw"; }
                { name = "see | english.stackexchange.com"; url = "https://english.stackexchange.com/"; keyword = "see"; }
                { name = "sej | japanese.stackexchange.com"; url = "https://japanese.stackexchange.com/"; keyword = "sej"; }
                { name = "su | superuser.com"; url = "https://superuser.com/"; keyword = "su"; }
                { name = "sef | money.stackexchange.com"; url = "https://money.stackexchange.com/"; keyword = "sef"; }
                { name = "ses | software.stackexchange.com"; url = "https://softwareengineering.stackexchange.com/"; keyword = "ses"; }
                { name = "col | numbeo.com/cost-of-living"; url = "https://www.numbeo.com/cost-of-living/"; keyword = "col"; }
                { name = "sd | sciencedaily.com"; url = "https://www.sciencedaily.com/"; keyword = "sd"; }
                { name = "sc | Student Center"; url = "https://hkust.edu.hk/stu_intranet/"; keyword = "sc"; }
                { name = "ac | Air Conditioner"; url = "https://w5.ab.ust.hk/njggt/app/"; keyword = "ac"; }
                { name = "sp | USTSpace"; url = "https://ust.space/home"; keyword = "sp"; }
                { name = "lt | Laundry Tickets"; url = "https://laundry.ust.hk/ldr/app/tickets"; keyword = "lt"; }
                { name = "pa | Path Advisor"; url = "https://pathadvisor.ust.hk/"; keyword = "pa"; }
                { name = "ol | Outlook"; url = "https://outlook.office.com/mail/"; keyword = "ol"; }
                { name = "ch | ArchChinese"; url = "https://www.archchinese.com/"; keyword = "ch"; }
                { name = "ca | Canvas"; url = "https://canvas.ust.hk"; keyword = "ca"; }
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
                    background-color: ${config.colors.colorBlue1} !important;
                    color: ${config.colors.colorWhite2} !important;
                }
            '';
        };
        programs.zsh.envExtra = builtins.readFile (dotfile "zsh/envExtra.zsh");
        programs.zsh.initExtra = builtins.readFile (dotfile "zsh/initExtra.zsh");

        home.file.".Rprofile".source = dotfile "Rprofile";

        # xmobar setup
        xdg.configFile."xmobar/xmobarrc".text = config.xmobarOptions;

        # neofetch setup
        xdg.configFile."neofetch/config.conf".source = dotfile "neofetch.conf";

        # mimeapps handling
        xdg.configFile."mimeapps.list".force = true;

        # khal configuration
        xdg.configFile."khal/config".source = dotfile "khal.config.ini";

        # account icon setting
        home.file.".face".source = dotfile "account-icon.png";

        xdg.dataFile = builtins.listToAttrs (
            # typst libraries
            lib.lists.flatten (
                lib.lists.forEach
                (lib.filesystem.listFilesRecursive (dotfile "typst-libraries"))
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
        ) // {
            # LaTeX libraries
            "latex".source = dotfile "latex-libraries";
        };

        # keynav setup
        xdg.configFile."keynav/keynavrc".source = dotfile "keynavrc";
    };
}
