{ firefox-pkgs, config, ... }:
{
    programs = {
        firefox = {
            enable = true;
            profiles.default = {
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
                    "media.hardware-video-decoding.enabled" = true;
                    "layout.css.devPixelsPerPx" = config.hyprland.firefoxScale;
                    "layout.css.dpi" = 96;
                };
                extensions = with firefox-pkgs; [
                    darkreader
                    vimium
                    adblocker-ultimate
                    export-cookies-txt
                ];
                search = {
                    force = true;
                    default = "Google";
                    order = [ "Google" "Searx" ];
                };
            };
            profiles.default = {
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
                    { name = "ps | pshash"; url = "https://thornoar.github.io/pshash/web/app/"; keyword = "ps"; }
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
        };
    };
}
