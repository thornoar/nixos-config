{ lib, config, ... }:

let
  dotfile = str: lib.path.append ../src str;
  ts = builtins.toString;
  toHaskell = mkstr:
    lib.attrsets.foldlAttrs (str: k: v:
      str + ''
        ${k} = ${if mkstr then ''"'' + ts v + ''"'' else ts v}
      '');
in {
  config = {
    # Xmonad configuration
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
      setWallpaperCmd = "hsetroot -cover $MEDIA/wallpapers/${config.wallpaper.dir}/$(ls $MEDIA/wallpapers/${config.wallpaper.dir} | shuf -n 1) -gamma ${
        ts config.wallpaper.gamma
      } -contrast ${ts config.wallpaper.contrast}"
    '' config.misc;
    xdg.configFile."xmonad/xmonad.hs".source = dotfile "xmonad.hs";

    # xmobar configuration
    xdg.configFile."xmobar/xmobarrc".text = config.xmobarOptions;

    # Kitty configuration
    xdg.configFile."kitty/minimal.lua".source = dotfile "kitty/minimal.lua";
    programs.kitty = {
      enable = true;
      font = {
        size = config.xmonad.fontsize;
        name = "Hack";
      };
      shellIntegration = {
        mode = "no-cursor";
        enableZshIntegration = true;
      };
      settings = {
        cursor = config.colors.fgColor0;
        cursor_shape = "underline";
        cursor_underline_thickness = "2.0";
        cursor_beam_thickness = "2.0";
        window_padding_width = ts config.xmonad.terminalPadding;
        foreground = config.colors.fgColor0;
        background = config.colors.bgColor0;
        background_opacity = ts config.xmonad.terminalOpacity;
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
        mouse_hide_wait = "1.0";
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

    # keynav configuration
    xdg.configFile."keynav/keynavrc".source = dotfile "keynavrc";

    # Transparent cursor theme
    home.file.".local/share/icons/transparent".source = dotfile "transparent";

    # account icon setting
    home.file.".face".source = dotfile "account-icon.png";
  };
}
