{ config, pkgs, inputs, system, lib, pkgs-unstable, readFile, readPackages, ... }:

{
  home.packages = with pkgs;
    [
      (python3.withPackages
        (ps: with ps; [ manim ipython sympy numpy ollama openai ]))
      broot
      nvd
    ];
}
