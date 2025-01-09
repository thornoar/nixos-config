{ lib, config, ... }:

let
    dotfile = str: lib.path.append ../src str;
    ts = builtins.toString;
    # toLua = lib.attrsets.foldlAttrs (str: k: v: str + "M.${k} = \"${ts v}\"\n");
    toHaskell = mkstr: lib.attrsets.foldlAttrs (str: k: v: str + "${k} = ${if mkstr then "\"" + ts v + "\"" else ts v}\n");
    toCSS = mkstr: lib.attrsets.foldlAttrs (str: k: v: str + "@define-color ${k} ${if mkstr then "\"" + ts v + "\"" else ts v};\n");
    toConf = mkstr: lib.attrsets.foldlAttrs (str: k: v: let v' = lib.strings.stringAsChars (x: if x == "#" then "0xff" else x) (ts v);
                                                        in str + "\$${k} = ${if mkstr then "\"" + v' + "\"" else v'}\n");
in 
{
    xdg.configFile."colors.css".text = (toCSS false "" config.colors);

    # Hyprland setup
    xdg.configFile."hypr/imports.conf".text = (toConf false "" config.hyprland) + (toConf false "" config.misc) + (toConf false "" config.colors);
    xdg.configFile."wpaperd/config.toml".text = ''
        [${config.misc.monitorName}]
        path = "/home/ramak/media/wallpapers/${config.wallpaper.dir}"
        duration = "3m"
    '';
    # xdg.configFile."hypr/hyprpaper.conf".source = dotfile "hypr/hyprpaper.conf";
    xdg.configFile."hypr/hyprland.conf".source = dotfile "hypr/hyprland.conf";

    # # Wofi setup
    # xdg.configFile."wofi/size.css".text = ''
    #     * {
    #         font-size: ${ts config.hyprland.fontsize}pt;
    #         border-radius: ${ts config.hyprland.rounding}px;
    #     }
    # '';
    # xdg.configFile."wofi/style.css".source = dotfile "wofi/style.css";

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
    xdg.configFile."xmonad/lib/Misc.hs".text = toHaskell true ''
        module Misc where
        setWallpaperCmd = "hsetroot -cover $MEDIA/wallpapers/${config.wallpaper.dir}/$(ls $MEDIA/wallpapers/${config.wallpaper.dir} | shuf -n 1) -gamma ${ts config.wallpaper.gamma} -contrast ${ts config.wallpaper.contrast}"
    '' config.misc;
    xdg.configFile."xmonad/xmonad.hs".source = dotfile "xmonad.hs";

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

    # xmobar setup
    xdg.configFile."xmobar/xmobarrc".text = config.xmobarOptions;

    # account icon setting
    home.file.".face".source = dotfile "account-icon.png";

    # keynav setup
    xdg.configFile."keynav/keynavrc".source = dotfile "keynavrc";

    # Transparent cursor theme
    home.file.".local/share/icons/transparent".source = dotfile "transparent";

    # zathura setup
    xdg.configFile."zathura/zathurarc".text = ''
        set window-title-basename "true"
        set selection-clipboard "clipboard"

        set notification-error-bg       "${config.colors.colorRed0}" # Red
        set notification-error-fg       "${config.colors.colorWhite1}" # Foreground
        set notification-warning-bg     "${config.colors.colorOrange0}" # Orange
        set notification-warning-fg     "${config.colors.bgColor2}" # Selection
        set notification-bg             "${config.colors.bgColor0}" # Background
        set notification-fg             "${config.colors.colorWhite1}" # Foreground
        set completion-bg               "${config.colors.bgColor0}" # Background
        set completion-fg               "${config.colors.colorBlue2}" # Comment
        set completion-group-bg         "${config.colors.bgColor0}" # Background
        set completion-group-fg         "${config.colors.colorBlue2}" # Comment
        set completion-highlight-bg     "${config.colors.bgColor2}" # Selection
        set completion-highlight-fg     "${config.colors.colorWhite1}" # Foreground
        set index-bg                    "${config.colors.bgColor0}" # Background
        set index-fg                    "${config.colors.colorWhite1}" # Foreground
        set index-active-bg             "${config.colors.bgColor2}" # Current Line
        set index-active-fg             "${config.colors.colorWhite1}" # Foreground
        set inputbar-bg                 "${config.colors.bgColor0}" # Background
        set inputbar-fg                 "${config.colors.colorWhite1}" # Foreground
        set statusbar-bg                "${config.colors.bgColor0}" # Background
        set statusbar-fg                "${config.colors.colorWhite1}" # Foreground
        set highlight-color             "${config.colors.colorOrange0}" # Orange
        set highlight-active-color      "${config.colors.colorMagenta0}" # Pink
        set default-bg                  "${config.colors.bgColor0}" # Background
        set default-fg                  "${config.colors.colorWhite1}" # Foreground
        set render-loading              true
        set render-loading-fg           "${config.colors.bgColor0}" # Background
        set render-loading-bg           "${config.colors.colorWhite1}" # Foreground

        set recolor-lightcolor          "${config.colors.bgColor0}" # Background
        set recolor-darkcolor           "${config.colors.colorWhite1}" # Foreground

        set adjust-open width
        # set recolor true
        set guioptions none

        map <S-Up> feedkeys "zI"
        map <S-Down> feedkeys "zO"
        map <M-Up> feedkeys "<PageUp>"
        map <M-Down> feedkeys "<PageDown>"

        set zoom-max 50000
    '';

    xdg.configFile."tofi/config".text = ''
        font-size = ${ts config.hyprland.fontsize}
        text-color = ${config.colors.colorWhite1}
        prompt-color =${config.colors.colorWhite1} 
        placeholder-color = ${config.colors.bgColor3}
        input-color =${config.colors.colorWhite1} 
        default-result-color =${config.colors.colorWhite1} 
        selection-color = ${config.colors.colorMagenta1}
        selection-background = ${config.colors.bgColor0}
        selection-match-color = ${config.colors.colorMagenta0}
        width = ${ts (config.hyprland.widthPixels / 2 - 2 * config.hyprland.windowSpaceOuter)}
        height = 50%
        background-color =${config.colors.bgColor0} 
        outline-width = 0
        outline-color =${config.colors.bgColor0} 
        border-width = 1
        border-color =${config.colors.colorMagenta1} 
        corner-radius = ${ts config.hyprland.rounding}
        padding-top = ${ts (config.hyprland.windowSpaceOuter / 2)}
        padding-bottom = ${ts (config.hyprland.windowSpaceOuter / 2)}
        padding-left = ${ts config.hyprland.windowSpaceOuter}
        padding-right = ${ts config.hyprland.windowSpaceOuter}
        margin-top = ${ts config.hyprland.windowSpaceOuter}
        margin-bottom = 0
        margin-left = ${ts config.hyprland.windowSpaceOuter}
        margin-right = ${ts config.hyprland.windowSpaceOuter}

    '' + builtins.readFile (dotfile "tofi.conf");
}
