{ pkgs, ... }:

{
  home.packages = (with pkgs;
    [
      (python3.withPackages
        (ps: with ps; [ ipython sympy numpy ollama openai ]))
      nvd
      tmux
      fzf
    ]);
}
