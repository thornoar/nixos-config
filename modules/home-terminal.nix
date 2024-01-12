{ system, config, pkgs, inputs, ... }:

{
    programs.alacritty = {
        enable = true;
        settings = {
            colors.bright = {
                black = "0x5c6370";
                blue = "0x61afef";
                cyan = "0x56b6c2";
                green = "0x329c48";
                magenta = "0xc678dd";
                red = "0xe06c75";
                white = "0xe6efff";
                yellow = "0xd19a66";
            };
            colors.dim = {
                black = "0x1e2127";
                blue = "0x61afef";
                cyan = "0x56b6c2";
                green = "0x329c48";
                magenta = "0xc678dd";
                red = "0xe06c75";
                white = "0x828791";
                yellow = "0xd19a66";
            };
            colors.normal = {
                black = "0x1e2127";
                blue = "0x61afef";
                cyan = "0x56b6c2";
                green = "0x329c48";
                magenta = "0xc678dd";
                red = "0xe06c75";
                white = "0xffffff";
                yellow = "0xd19a66";
            };
            colors.primary = {
                background = "0x1e2127";
                bright_foreground = "0x00bc96";
                foreground = "0x17a88b";
            };
            cursor = {
                style = "Underline";
            };
            env = {
                COLORTERM = "truecolor";
                TERM = "xterm-256color";
            };
            font = {
                size = config.fontsize;
            };
            font.bold = {
                family = "Hack";
                style = "Bold";
            };
            font.bold_italic = {
                family = "Hack";
                style = "Bold Italic";
            };
            font.italic = {
                family = "Hack";
                style = "Italic";
            };
            font.normal = {
                family = "Hack";
                style = "Regular";
            };
            window.padding = {
                x = 6;
                y = 5;
            };
        };
    };
    # xdg.configFile."alacritty".source = ../dotfiles/alacritty;

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
            PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "
        '';
        shellAliases = rec {
            rb = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/";
            rb-home = "home-manager switch --impure --flake $NIXOS_CONFIG/";
            rb-system = "sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master";
            rb-boot = "sudo nixos-rebuild boot --impure --flake $NIXOS_CONFIG/#master";
            rb-both = "${rb}#master && ${rb-home}";
            fullrb = "${rb-both} && ${gc}";
            gc = "nix-collect-garbage --delete-old && sudo nix-collect-garbage --delete-old";
            fullgc = "${gc} && ${rb-boot}";
            hm = "home-manager";
            sncfg = "${sc} && git fetch && git pull";
            pushcfg = "${sc} && git add . && git commit -m '--' && git push";
            ns = "nix-shell --command zsh -p ";
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

# PS1="[%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "
