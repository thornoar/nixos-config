{ pkgs, config, ... }:

let
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
      name = "qw | Outlook";
      url = "https://outlook.office365.com/mail/";
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
    {
      name = "sc | Student Center";
      url = "https://hkust.edu.hk/stu_intranet/";
      keyword = "sc";
    }
    {
      name = "ac | Air Conditioner";
      url = "https://w5.ab.ust.hk/njggt/app/";
      keyword = "ac";
    }
    {
      name = "sp | USTSpace";
      url = "https://ust.space/home";
      keyword = "sp";
    }
    {
      name = "lt | Laundry Tickets";
      url = "https://laundry.ust.hk/ldr/app/tickets";
      keyword = "lt";
    }
    {
      name = "pa | Path Advisor";
      url = "https://pathadvisor.ust.hk/";
      keyword = "pa";
    }
    {
      name = "ch | ArchChinese";
      url = "https://www.archchinese.com/";
      keyword = "ch";
    }
    {
      name = "ca | Canvas";
      url = "https://canvas.ust.hk";
      keyword = "ca";
    }
    {
      name = "ps | pshash";
      url = "https://thornoar.github.io/pshash/web/app/";
      keyword = "ps";
    }
    {
      name = "ds | problem-distribution";
      url = "https://thornoar.github.io/problem-distribution/";
      keyword = "ds";
    }
    {
      name = "sg | sage documentation";
      url = "https://doc.sagemath.org/html/en/tutorial/index.html";
      keyword = "sg";
    }
    {
      name = "rs | rust documentation";
      url = "https://doc.rust-lang.org/rust-by-example/";
      keyword = "rs";
    }
    {
      name = "clj | clojure documentation";
      url = "https://clojure.org/guides/getting_started";
      keyword = "clj";
    }
    {
      name = "ftl | fasterthanli";
      url = "https://fasterthanli.me/";
      keyword = "ftl";
    }
    {
      name = "c++ | COMP2011";
      url = "https://course.cse.ust.hk/comp2011";
      keyword = "c++";
    }
    {
      name = "ww | MATH2023";
      url = "https://webwork.math.ust.hk/webwork2";
      keyword = "ww";
    }
    {
      name = "ag | appr-game";
      url = "https://thornoar.github.io/appr-game/";
      keyword = "ag";
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
        background-color: ${config.colors.colorBlue1} !important;
        color: ${config.colors.colorWhite2} !important;
    }
  '';
  extensions = with pkgs.firefox-pkgs; [
    darkreader
    vimium
    adblocker-ultimate
    export-cookies-txt
  ];
  search = {
    force = true;
    default = "google";
    order = [ "google" "searx" ];
  };
  baseSettings = {
    "browser.startup.homepage" = "about:home";
    "browser.tabs.inTitlebar" = 0;
    "browser.toolbars.bookmarks.visibility" = "never";
    "browser.search.defaultenginename" = "Google";
    "browser.search.order.1" = "Google";
    "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    "signon.rememberSignons" = false;
    "media.hardware-video-decoding.enabled" = true;
    "layout.css.dpi" = 96;
  };
  baseProfile = {
    inherit search;
    inherit userChrome;
    extensions.packages = extensions;
    bookmarks = {
      force = true;
      settings = bookmarks;
    };
  };
in {
  programs = {
    firefox = {
      enable = true;
      profiles.hyprland = baseProfile // {
        id = 0;
        name = "hyprland";
        isDefault = true;
        settings = baseSettings // {
          "layout.css.devPixelsPerPx" = config.hyprland.firefoxScale;
        };
      };
      profiles.hyprland-clean = baseProfile // {
        id = 3;
        name = "hyprland-clean";
        isDefault = false;
        settings = baseSettings // {
          "layout.css.devPixelsPerPx" = config.hyprland.firefoxScale;
        };
      };
      # profiles.xmonad = baseProfile // {
      #   id = 1;
      #   name = "xmonad";
      #   isDefault = false;
      #   settings = baseSettings // {
      #     "layout.css.devPixelsPerPx" = config.xmonad.firefoxScale;
      #   };
      # };
    };
  };
}
