{ config, pkgs, lib, pkgs-unstable, ... }:

{
    imports = [
        ../home-options.nix
        /home/ramak/projects/nixos-local-config/home-local.nix
        ../home-scripts.nix
        ./external.nix
    ];

    config = 
    let 
        my-python-packages = ps: with ps; [
            ipython
            sympy
            numpy
            pandas
            matplotlib
        ];
        my-julia-packages = [];
    in
    {
        home.username = "ramak";
        home.homeDirectory = "/home/ramak";
        xdg.userDirs = {
            enable = true;
            download = "${config.home.homeDirectory}/dls";
            desktop = "${config.home.homeDirectory}/dsk";
            documents = "${config.home.homeDirectory}/docs";
            pictures = "${config.home.homeDirectory}/media/pictures";
        };
        home.sessionVariables = {
            BAT_THEME = "base16";
        };

        home.file.".local/share/applications/inkview.desktop".text = ''
            [Desktop Entry]
            Type=Application
            Name=Inkview
            Comment=View SVG files
            Exec=inkview %U
            Categories=Graphics;2DGraphics;
            MimeType=image/svg+xml;
        '';
        xdg.mimeApps = rec {
            enable = true;
            associations.added = {
                "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
                "audio/mpeg" = [ "mpv.desktop" ];
                "audio/mp3" = [ "mpv.desktop" ];
                "video/vnd.avi" = [ "mpv.desktop" ];
                "image/vnd.djvu+multipage" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
                "image/svg+xml" = [ "inkview.desktop" ];
            };
            defaultApplications = associations.added;
        };

        nixpkgs.config.allowUnfree = true;

        home.packages = (lib.lists.forEach (lib.lists.partition (x: 0 < lib.strings.stringLength x) 
		(lib.strings.splitString "\n" (builtins.readFile ./packages.txt))).right (name: pkgs.${name}))
		++ (with pkgs; [
			texlive.combined.scheme-full
			(python3.withPackages my-python-packages)
            manim
            R
            cargo
            rustc
            ghc
            cabal-install
            lua
            nodejs
            julia
            openjdk17-bootstrap
		])
        ++ (with pkgs-unstable; [
            fzf
            # (julia.withPackages my-julia-packages) # Maybe in 24.05
        ]);
        # [./packages.txt]

        programs = {
            zsh = {
                enable = true;
                enableCompletion = true;
                enableAutosuggestions = true;
                syntaxHighlighting.enable = true;
                shellAliases = {
                    ns = "nix-shell --command zsh -p ";
                    wget = "wget --hsts-file = $XDG_DATA_HOME/wget-hsts";
                    trr = "transmission-remote";
                    film = "transmission-remote -w ~/media/films -a ";
                    music = "transmission-remote -w ~/media/music -a ";
                    la = "exa -lAh";
                    open = "xdg-open";
                    svim = "sudo -E nvim";
                    quit = "exit";
                    close = "exit";
                    grep = "grep --color=auto";
                    sxiv = "sxiv -b";
                    def = "dict -h dict.org";
                    clip = "xclip -r -selection c";
                    vmcon = "virt-manager --connect qemu:///system --show-domain-console";
                    vmstart = "sudo virsh start";
                    vmstop = "sudo virsh shutdown";
                    commit = "git add .; git commit -m \"auto\"";
                };    
                initExtra = ''
                    autoload -U colors && colors
                    # PS1="%{$fg[magenta]%}[%{$fg[blue]%}%n%{$fg[magenta]%}] %{$fg[yellow]%}%~ %{$fg[magenta]%}:%{$reset_color%} "
                    PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}: "

                    function preexec() {
                        timer=$(($(date +%s%0N)/1000000))
                    }

                    function precmd() {
                        echo -ne '\e[4 q'
                        if [ $timer ]; then
                        now=$(($(date +%s%0N)/1000000))
                        elapsed=$(($now-$timer))

                        # export RPROMPT="%F{yellow}''${elapsed}ms%{$reset_color%}"
                        export RPROMPT="< %{$fg[yellow]%}''${elapsed}ms%{$reset_color%}"
                        unset timer
                        fi
                    }

                    source $NIXOS_CONFIG/dotfiles/br.sh
                    # eval $(thefuck --alias)

                    bindkey "^[[1;3D" backward-word 
                    bindkey "^[[1;3C" forward-word

                    typeset -U PATH path
                    BINPATH="$PROJECTS"
                    path+=("$BINPATH" "''${BINPATH}"/*/bin)
                    export PATH

                    eval "$(fzf --zsh)"
                    bindkey "^[[1;5B" fzf-file-widget
                    bindkey "^[[1;5C" fzf-cd-widget
                    bindkey "^[[1;5A" fzf-history-widget
                    export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
                    export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
                    _fzf_comprun() {
                        local command=$1
                        shift

                        case "$command" in
                            cd) fzf --preview "eza --tree --color=always {} | head -200" "$@" ;;
                            export|unset) fzf --preview "eval 'echo \$'{}" "$@" ;;
                            ssh) fzf --preview "dig {}" "$@" ;;
                            *) fzf --preview "--preview 'bat -n --color=always --line-range :500 {}'" "$@" ;;
                        esac
                    }

                    TIMEFMT=$'\n'\
                    'time:          %U user %S system %P cpu %*E total'$'\n'\
                    'max memory:    %M '$MAX_MEMORY_UNITS'MB'
                '';
            };
            git = {
                enable = true;
                userName = "Roman Maksimovich";
                userEmail = "r.a.maksimovich@gmail.com";
                extraConfig = {
                    init.defaultBranch = "master";
                };
            };
            gh = { enable = true; };
            tmux = {
                enable = true;
                extraConfig = ''
                    set -g prefix C-q
                    unbind-key C-b
                    bind-key C-q send-prefix
                    set -g default-terminal screen-256color
                '';
            };
        };

        xresources.properties = {
            "Xcursor.size" = 16;
            "Xcursor.theme" = "Adwaita";
        };
        home.stateVersion = "23.11";
        programs.home-manager.enable = true;

        dconf.settings = {
            "org/virt-manager/virt-manager/connections" = {
                autoconnect = ["qemu:///system"];
                uris = ["qemu:///system"];
            };
        };

        gtk = {
            enable = true;
            font.name = "Hack Mono 11";
            theme = {
                name = "deepin-dark";
                package = pkgs.deepin.deepin-gtk-theme;
            };
        };
    };
}
