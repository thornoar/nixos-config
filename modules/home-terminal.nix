{ system, config, pkgs, inputs, ... }:

{
    programs.alacritty = {
        enable = true;
        settings = {
            colors.bright = {
                black = "#5c6370";
                blue = "${config.colorBlue_alt}";
                cyan = "#56b6c2";
                green = "#329c48";
                magenta = "${config.colorMagenta_alt}";
                red = "#e06c75";
                white = "#e6efff";
                yellow = "#d19a66";
            };
            colors.dim = {
                black = "#1e2127";
                blue = "${config.colorBlue_alt}";
                cyan = "#56b6c2";
                green = "#329c48";
                magenta = "${config.colorMagenta_alt}";
                red = "#e06c75";
                white = "#828791";
                yellow = "#d19a66";
            };
            colors.normal = {
                black = "#1e2127";
                blue = "${config.colorBlue_alt}";
                cyan = "#56b6c2";
                green = "#329c48";
                magenta = "${config.colorMagenta_alt}";
                red = "#e06c75";
                white = "#ffffff";
                yellow = "#d19a66";
            };
            colors.primary = {
                background = config.bgColor;
                bright_foreground = config.brfgColor;
                foreground = config.fgColor;
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
                family = config.font;
                style = "Bold";
            };
            font.bold_italic = {
                family = config.font;
                style = "Bold Italic";
            };
            font.italic = {
                family = config.font;
                style = "Italic";
            };
            font.normal = {
                family = config.font;
                style = "Regular";
            };
            window.padding = {
                x = config.padding.x;
                y = config.padding.y;
            };
            keyboard.bindings = [
                { key = "PageUp"; action = "ScrollLineUp"; }
                { key = "PageDown"; action = "ScrollLineDown"; }
            ];
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
        # prezto.prompt.theme = "pure";
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
            fullrb = "${rb-both} && ${gc} && xmonad --recompile && xmonad --restart";
            gc = "nix-collect-garbage --delete-old && sudo nix-collect-garbage --delete-old";
            fullgc = "${gc} && ${rb-boot}";
            hm = "home-manager";
            gitpush = "git add . && git commit -m '--' && git push";
            gitpull = "git fetch && git pull";
            ns = "nix-shell --command zsh -p ";
            cdir = "cd ~/.config/nvim";
            sbdir = "cd ~/projects/sandbox";
            media = "cd ~/media";
            films = "cd ~/media/Films";
            books = "cd ~/media/Books";
            wget = "wget --hsts-file = $XDG_DATA_HOME/wget-hsts";
            cp = "cp -i";
            tr-remote = "transmission-remote";
            film = "transmission-remote -w ~/media/films -a ";
            music = "transmission-remote -w ~/media/music -a ";
            c = "ping google.com";
            la = "exa -la";
            open = "xdg-open";
            svim = "sudo -E nvim";
            sc = "cd $NIXOS_CONFIG";
        };    
    };
}
