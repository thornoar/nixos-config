{ lib, config, ... }:

let
    dotfile = str: lib.path.append ../src str;
    ts = builtins.toString;
    toCSS = mkstr: lib.attrsets.foldlAttrs (str: k: v: str + "@define-color ${k} ${if mkstr then "\"" + ts v + "\"" else ts v};\n");
in 
{
    xdg.configFile."colors.css".text = (toCSS false "" config.colors);

    # mpv configuration
    xdg.configFile."mpv/mpv.conf".text = ''
        loop-file=inf
        sub-file-paths=RusSubs:subs:subtitles
    '';

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
}
