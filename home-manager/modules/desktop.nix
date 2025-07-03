{ pkgs, pkgs-unstable, ... }:

{
  home.packages = (with pkgs;
    [
      (python3.withPackages
        (ps: with ps; [ manim ipython sympy numpy ollama openai ]))
      broot
      nvd
      tmux
    ]) ++ (with pkgs-unstable;
    [
      fzf
    ]);
}
