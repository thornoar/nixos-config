{ system, config, pkgs, inputs, ... }:

{
    programs.alacritty = {
        enable = true;
    };
    xdg.configFile."alacritty".source = ../dotfiles/alacritty;

    programs.neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
    };
    xdg.configFile."nvim".source = ../dotfiles/nvim;

    programs.zsh = {
        enable = true;
        enableCompletion = true;
        enableAutosuggestions = true;
        syntaxHighlighting.enable = true;
        prezto.prompt.theme = "pure";
        initExtra = ''
            autoload -U colors && colors
            PS1="[%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "
        '';
        shellAliases = rec {
            rb = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/";
            rb-home = "home-manager switch --flake $NIXOS_CONFIG/";
            rb-master = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master";
            rb-boot = "sudo nixos-rebuild boot --impure --flake $NIXOS_CONFIG/#master";
            rb-both = "${rb}#master && ${rb-home}";
            fullrb = "${rb-both} && ${gc}";
            gc = "sudo nix-collect-garbage --delete-old";
            fullgc = "${gc} && ${rb-boot}";
            hm = "home-manager";
            cdir = "cd ~/.config/nvim";
            sbdir = "cd ~/projects/sandbox";
            media = "cd ~/media";
            films = "cd ~/media/Films";
            books = "cd ~/media/Books";
            up = "sudo pacman -Syu";
            get = "sudo pacman -S --noconfirm";
            q = "exit";
            getsetup = "pacman -Qqe";
            ls = "ls $LS_OPTIONS";
            wget = "wget --hsts-file = $XDG_DATA_HOME/wget-hsts";
            cp = "cp -i";
            df = "df -h";
            free = "free -m";
            gitu = "git add . && git commit && git push";
            dgit = "cd $HOME/Downloads/git";
            tr = "transmission-remote";
            film = "transmission-remote -w ~/media/Films -a ";
            music = "transmission-remote -w ~/media/Music -a ";
            w3mimgdisplay = "w3m";
            c = "ping google.com";
            ll = "ls -la";
            open = "xdg-open";
            zc = "zle-keymap-select";
            svim = "sudo -E nvim";
            sc = "cd $NIXOS_CONFIG";
        };
    };
}
