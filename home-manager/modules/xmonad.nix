{ config, ... }:

let
  ts = builtins.toString;
in {
  # Xmonad configuration
  xdg.configFile."xmonad/lib/Colors.hs".text = config.util.toHaskell true ''
    module Colors where
  '' config.colors;
  # xdg.configFile."xmonad/lib/Size.hs".text = config.util.toHaskell false ''
  #   module Size where
  #   import XMonad
  #   import Data.Ratio
  #   barHeight :: Dimension
  #   windowSpaceInner :: Int
  #   windowSpaceOuter :: Int
  #   windowBorderWidth :: Dimension
  # '' config.window;
  xdg.configFile."xmonad/lib/Misc.hs".text = config.util.toHaskell true ''
    module Misc where
    setWallpaperCmd = "hsetroot -cover $MEDIA/wallpapers/${config.wallpaper.dir}/$(ls $MEDIA/wallpapers/${config.wallpaper.dir} | shuf -n 1) -gamma ${
      ts config.wallpaper.gamma
    } -contrast ${ts config.wallpaper.contrast}"
  '' config.misc;
  xdg.configFile."xmonad/xmonad.hs" = config.util.dotFileMut "xmonad/xmonad.hs";

  # xmobar configuration
  # xdg.configFile."xmobar/xmobarrc".text = config.xmobarOptions;

  # Kitty configuration
  # xdg.configFile."kitty/minimal.lua" = config.util.dotFileMut "kitty/minimal.lua";
  programs.kitty = {
    enable = true;
    font = {
      size = config.window.fontsize;
      name = "Hack";
    };
    shellIntegration = {
      mode = "no-cursor";
      enableZshIntegration = true;
    };
    settings = {
      cursor = config.colors.fg0;
      cursor_shape = "underline";
      cursor_underline_thickness = "2.0";
      cursor_beam_thickness = "2.0";
      window_padding_width = ts config.window.terminalPadding;
      foreground = config.colors.fg0;
      background = config.colors.bg0;
      background_opacity = ts config.window.terminalOpacity;
      color0 = config.colors.bg0;
      color8 = config.colors.bg3;
      color1 = config.colors.red1;
      color9 = config.colors.red1;
      color2 = config.colors.green1;
      color10 = config.colors.green1;
      color3 = config.colors.yellow1;
      color11 = config.colors.yellow1;
      color4 = config.colors.blue1;
      color12 = config.colors.blue1;
      color5 = config.colors.magenta1;
      color13 = config.colors.magenta1;
      color6 = config.colors.cyan;
      color14 = config.colors.cyan;
      color7 = config.colors.white1;
      color15 = config.colors.bg3;
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
      # scrollback_pager nvim --noplugin -u ~/.config/kitty/minimal.lua -c "START"

      # action_alias kitty_scrollback_nvim kitten /home/ramak/.local/share/nvim/lazy/kitty-scrollback.nvim/python/kitty_scrollback_nvim.py
      # map kitty_mod+h kitty_scrollback_nvim
      # map kitty_mod+g kitty_scrollback_nvim --config ksb_builtin_last_cmd_output
      # mouse_map ctrl+shift+right press ungrabbed combine : mouse_select_command_output : kitty_scrollback_nvim --config ksb_builtin_last_visited_cmd_output
    '';
  };

  # keynav configuration
  xdg.configFile."keynav/keynavrc" = config.util.dotFileMut "keynavrc";
}
