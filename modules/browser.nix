{ system, config, pkgs, inputs, ... }:

{
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
                };
                extensions = with inputs.firefox-addons.packages.${system}; [
                    darkreader
                    vimium
                ];
                bookmarks = [
                    {
                        name = "YouTube";
                        url = "https://youtube.com";
                        keyword = "yt";
                    }
                    {
                        name = "YTMusic";
                        url = "https://music.youtube.com";
                        keyword = "ym";
                    }
                    {
                        name = "GitHub";
                        url = "https://github.com/thornoar";
                        keyword = "gh";
                    }
                    {
                        name = "Work Mail";
                        url = "https://mail.google.com/mail/u/1/#inbox";
                        keyword = "qw";
                    }
                    {
                        name = "Personal Mail";
                        url = "https://mail.google.com/mail/u/0/#inbox";
                        keyword = "as";
                    }
                    {
                        name = "MyNixOS";
                        url = "https://mynixos.com";
                        keyword = "mn";
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
                    background-color: #0078d7 !important;
                    color: #fff !important;
                    }
                '';
            };
        };
    };
}
