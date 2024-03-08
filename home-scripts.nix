{ config, pkgs, lib, ... }:

{
    home.packages = lib.lists.forEach (lib.filesystem.listFilesRecursive ./scripts) (filename:
        pkgs.writeScriptBin
        (lib.strings.removeSuffix ".sh"  (
            lib.lists.last (lib.strings.splitString "/" (builtins.toString filename))
        ))
        (builtins.readFile filename)
    );
}

# [./scripts]
