{ lib, config, ... }:

let
  dotfile = str: lib.path.append ../src str;
  ts = builtins.toString;
  toLua = lib.attrsets.foldlAttrs (str: k: v:
    str + ''
      M.${k} = "${ts v}"
    '');
in {
}
