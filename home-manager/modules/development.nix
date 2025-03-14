{ pkgs, pkgs-unstable, readPackages, ... }: {
  home.packages = with pkgs;
    [
      (python3.withPackages
        (ps: with ps; [ manim ipython sympy numpy ollama openai ]))
      (rWrapper.override {
        packages = with rPackages; [
          languageserver
          ggplot2
          dplyr
          xts
          pracma
          latex2exp
        ];
      })
      (texlive.combine { inherit (texlive) scheme-full; })
    ] ++ readPackages ../src/packages/development.txt pkgs;

  programs = {
    neovim = {
      enable = true;
      # package = pkgs-unstable.neovim;
      withPython3 = true;
      extraPython3Packages = ps: with ps; [ sympy pynvim ];
    };
    helix = {
      enable = true;
      package = pkgs-unstable.helix;
    };
    zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      shellAliases = {
        torrent = "transmission-remote";
        film = "transmission-remote -w ~/media/films -a ";
        music = "transmission-remote -w ~/media/music -a ";
        lbr = "clear; br";
        open = "xdg-open";
        close = "exit";
        grep = "grep --color=auto";
        def = "dict -h dict.org";
        vmcon = "virt-manager --connect qemu:///system --show-domain-console";
        vmstart = "sudo virsh start";
        vmstop = "sudo virsh shutdown";
        clip = "wl-copy -n";
        gpp = "g++ -std=c++11 -Wall";
        gitlog = "git log --oneline --graph --decorate";
      };
    };
    git = {
      enable = true;
      userName = "Roman Maksimovich";
      userEmail = "r.a.maksimovich@gmail.com";
      extraConfig = { init.defaultBranch = "master"; };
      aliases = {
        install = ''
          !git clone "https://github.com/thornoar/$1.git" "$HOME/projects/$1"'';
      };
    };
    gh = { enable = true; };
  };
}
