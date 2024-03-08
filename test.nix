let
    lib = import <nixpkgs/lib>;
in
    lib.lists.forEach (lib.filesystem.listFilesRecursive ./dotfiles/scripts) (filename:
        (lib.strings.removeSuffix ".sh"  (
            lib.lists.last (lib.strings.splitString "/" (builtins.toString filename))
        ))
    )
