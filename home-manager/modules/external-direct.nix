{ lib, config, ... }:

{
    config = 
    let
        dotfile = str: lib.path.append ./src str;
    in
    {
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

        # Broot configuration
    
        xdg.configFile."broot/conf.hjson".source = dotfile "broot/conf.hjson";
        xdg.configFile."broot/verbs.hjson".source = dotfile "broot/verbs.hjson";
        xdg.configFile."broot/colorscheme.hjson".text = ''
            skin: {
                default: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
                tree: "${config.colors.bgColor3} None / ${config.colors.bgColor2} None"
                parent: "${config.colors.colorYellow0} None / ${config.colors.colorYellow1} None"
                file: "None None / None  None Italic"
                directory: "${config.colors.colorBlue1} None Bold / ${config.colors.colorBlue1} None"
                exe: "${config.colors.colorGreen1} None"
                link: "${config.colors.colorGreen1} None"
                pruning: "${config.colors.colorWhite4} None"
                perm__: "None None"
                perm_r: "${config.colors.colorYellow0} None"
                perm_w: "${config.colors.colorRed1} None"
                perm_x: "${config.colors.colorOrange0} None"
                owner: "${config.colors.colorWhite2} None"
                group: "${config.colors.colorWhite3} None"
                count: "${config.colors.colorWhite4} None"
                size: "${config.colors.colorWhite4} None"
                dates: "${config.colors.colorWhite3} None"
                sparse: "${config.colors.colorYellow0} None"
                content_extract: "ansi(29) None"
                content_match: "${config.colors.colorMagenta0} None Bold"
                git_branch: "${config.colors.colorWhite2} None"
                git_insertions: "${config.colors.colorGreen4} None"
                git_deletions: "${config.colors.colorRed1} None"
                git_status_current: "${config.colors.bgColor2} None"
                git_status_modified: "${config.colors.colorBlue1} None"
                git_status_new: "${config.colors.colorGreen4} None Bold"
                git_status_ignored: "${config.colors.colorWhite3} None"
                git_status_conflicted: "${config.colors.colorRed1} None"
                git_status_other: "${config.colors.colorRed1} None"
                selected_line: "None ${config.colors.bgColor2} / None ${config.colors.bgColor2}"
                char_match: "${config.colors.colorMagenta3} None"
                file_error: "${config.colors.colorRed1} None"
                flag_label: "${config.colors.colorWhite4} None"
                flag_value: "${config.colors.colorMagenta3} None Bold"
                input: "${config.colors.colorWhite2} None / ${config.colors.colorWhite4} None"
                status_error: "${config.colors.colorWhite3} ${config.colors.colorRed0}"
                status_job: "${config.colors.colorYellow1} ${config.colors.bgColor1}"
                status_normal: "None ${config.colors.bgColor0} / None None"
                status_italic: "${config.colors.colorMagenta3} ${config.colors.bgColor0} / None None"
                status_bold: "${config.colors.colorMagenta3} ${config.colors.bgColor0} Bold / None None"
                status_code: "${config.colors.colorWhite2} ${config.colors.bgColor0} / None None"
                status_ellipsis: "${config.colors.colorWhite2} ${config.colors.bgColor0} Bold / None None"
                purpose_normal: "None None"
                purpose_italic: "${config.colors.colorMagenta3} None"
                purpose_bold: "${config.colors.colorMagenta3} None Bold"
                purpose_ellipsis: "None None"
                scrollbar_track: "${config.colors.bgColor0} None / ${config.colors.bgColor0} None"
                scrollbar_thumb: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
                help_paragraph: "None None"
                help_bold: "${config.colors.colorOrange1} None Bold"
                help_italic: "${config.colors.colorMagenta3} None"
                help_code: "${config.colors.colorGreen4} ${config.colors.bgColor1}"
                help_headers: "${config.colors.colorOrange1} None Bold"
                help_table_border: "${config.colors.bgColor0} None"
                preview_title: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
                preview: "${config.colors.colorWhite3} ${config.colors.bgColor0} / ${config.colors.colorWhite3} ${config.colors.bgColor0}"
                preview_line_number: "${config.colors.colorWhite4} None / ${config.colors.colorWhite4} None"
                preview_match: "None ${config.colors.bgColor0} Bold"
                hex_null: "${config.colors.colorWhite4} None"
                hex_ascii_graphic: "${config.colors.colorWhite3} None"
                hex_ascii_whitespace: "${config.colors.colorOrange0} None"
                hex_ascii_other: "${config.colors.colorOrange1} None"
                hex_non_ascii: "${config.colors.colorOrange1} None"
                staging_area_title: "${config.colors.colorWhite3} None / ${config.colors.colorWhite4} None"
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
                txt: "${config.colors.colorWhite3}"
                asy: "${config.colors.colorYellow1}"
                py: "${config.colors.colorCyan}"
                typ: "${config.colors.colorCyan}"
                hs: "${config.colors.colorMagenta2}"
                vim: "${config.colors.colorGreen2}"
                nix: "${config.colors.colorMagenta1}"
                rs: "${config.colors.colorOrange1}"
                lua: "${config.colors.colorBlue0}"
                pdf: "${config.colors.colorRed2}"
                epub: "${config.colors.colorRed2}"
                djvu: "${config.colors.colorRed2}"
                lock: "${config.colors.colorWhite0}"
                torrent: "${config.colors.colorGreen1}"
            }
        '';
    };
}
