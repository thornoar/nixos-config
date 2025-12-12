{ lib, config, ... }:

{
  options = {
    colors = lib.mkOption {
      type = lib.types.attrs;
      default = rec {
        primary = white3;

        bg0 = "#0c0c10";
        bg1 = "#121215";
        bg2 = "#262728";
        bg3 = "#4d4e4f";
        commentColor = "#5c6370";
        fg0 = "#17a88b";
        fg1 = "#00bc96";

        black = "#000000";
        # blackFg = commentColor;

        white0 = "#ffffff";
        white1 = "#f8f8f2";
        white2 = "#e6efef";
        white3 = "#abb2bf";
        white4 = "#595f6b";

        yellow0 = "#f1fa8c";
        yellow1 = "#d19a66";
        yellow2 = "#eebb9e";

        orange0 = "#ffb86c";
        orange1 = "#c28141";
        orange2 = "#d27141";

        red0 = "#ff5555";
        red1 = "#e86671";
        red2 = "#dd9999";

        blue0 = "#3070f0";
        blue1 = "#61afef";
        blue2 = "#6272a4";

        cyan = "#56b6c2";

        green0 = "#89a870";
        green1 = "#329c48";
        green2 = "#78971a";
        green4 = "#98c379";

        magenta0 = "#e772c1";
        magenta1 = "#bd93f9";
        magenta2 = "#c678dd";
        magenta3 = "#b36596";
      };
    };

    hyprland = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    wallpaper = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    misc = lib.mkOption {
      type = lib.types.attrs;
      default = {
        usePackageList = true;
        systemFont = "Hack";
      };
    };

    util = lib.mkOption {
      type = lib.types.attrs;
      default = let ts = builtins.toString; in rec {
        dotFile = mkLink: srcPath: path: {
          enable = true;
          recursive = true;
          source = mkLink (lib.path.append srcPath path);
        };

        srcPath = /home/ramak/projects/nixos-config/home-manager/src;

        dotFileImmut = dotFile (x: x) ../src;

        dotFileMut = dotFile config.lib.file.mkOutOfStoreSymlink srcPath;

        toCSS = mkstr:
          lib.attrsets.foldlAttrs (str: k: v:
            str + ''
              @define-color ${k} ${if mkstr then ''"'' + ts v + ''"'' else ts v};
            '');

        toConf = mkstr:
          lib.attrsets.foldlAttrs (str: k: v:
            let
              v' =
                lib.strings.stringAsChars (x: if x == "#" then "0xff" else x) (ts v);
            in str + ''
              ''$${k} = ${if mkstr then ''"'' + v' + ''"'' else v'}
            '');

        toLua = lib.attrsets.foldlAttrs (str: k: v:
          str + ''
            M.${k} = "${ts v}"
          ''
        );
      };
    };
  };
}
