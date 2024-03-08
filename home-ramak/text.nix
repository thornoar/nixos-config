let lib = import <nixpkgs/lib>; in lib.strings.splitString "\n" (builtins.readFile ./test.txt)
