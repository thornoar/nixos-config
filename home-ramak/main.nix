{ system, usrname, inputs, config, pkgs, lib, ... }:

{
    imports = [
        ../home-options.nix
        /etc/nixos/home-local.nix
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
    in
    {
        home.username = usrname;
        home.homeDirectory = "/home/"+usrname;
        xdg.userDirs = {
            enable = true;
            download = "${config.home.homeDirectory}/dls";
            documents = "${config.home.homeDirectory}/docs";
        };
        home.sessionPath = [ "$PROJECTS/password-hash" ];

        xdg.mimeApps = {
            enable = true;
            associations.added = {
                "application/pdf" = ["org.pwmt.zathura-pdf-mupdf.desktop"];
                "audio/mpeg" = ["mpv.desktop"];
                "audio/mp3" = ["mpv.desktop"];
                "video/vnd.avi" = ["mpv.desktop"];
                "image/vnd.djvu+multipage" = ["org.pwmt.zathura-pdf-mupdf.desktop"];
            };
            defaultApplications = {
                "application/pdf" = ["org.pwmt.zathura-pdf-mupdf.desktop"];
                "audio/mpeg" = ["mpv.desktop"];
                "audio/mp3" = ["mpv.desktop"];
                "video/vnd.avi" = ["mpv.desktop"];
                "image/vnd.djvu+multipage" = ["org.pwmt.zathura-pdf-mupdf.desktop"];
            };
        };

        nixpkgs.config.allowUnfree = true;

        home.packages = (lib.lists.forEach (lib.lists.partition (x: 0 < lib.strings.stringLength x) 
		(lib.strings.splitString "\n" (builtins.readFile ../home-packages))).right (name: pkgs.${name}))
		++ (with pkgs; [
            # haskellPackages.optparse-applicative
			texlive.combined.scheme-full
			(python3.withPackages my-python-packages)
		]);
        # [../home-packages]

        programs = {
            zsh = {
                enable = true;
                enableCompletion = true;
                enableAutosuggestions = true;
                syntaxHighlighting.enable = true;
                shellAliases = rec {
                    cd = "z";
                    rc = "nmcli con up 9a64ee51-26a1-4c77-9361-df3de07cbfab";
                    xc = "recompile_xmonad";
                    hm = "home-manager";
                    gst = "git status";
                    ns = "nix-shell --command zsh -p ";
                    cdir = "${cd} ~/.config/nvim";
                    sbdir = "${cd} ~/projects/sandbox";
                    media = "${cd} ~/media";
                    films = "${cd} ~/media/films";
                    books = "${cd} ~/media/books";
                    wget = "wget --hsts-file = $XDG_DATA_HOME/wget-hsts";
                    cp = "cp -i";
                    trr = "transmission-remote";
                    film = "transmission-remote -w ~/media/films -a ";
                    music = "transmission-remote -w ~/media/music -a ";
                    c = "ping google.com";
                    la = "exa -lAh";
                    open = "xdg-open";
                    svim = "sudo -E nvim";
                    sc = "${cd} $NIXOS_CONFIG";
                    calc = "qalc -c";
                    quit = "exit";
                    grep = "grep --color=auto";
                    fucking = "mommy";
                    q = "qalc -c";
                    sxiv = "sxiv -b";
                    lg = "lazygit";
                    ip = "ipython";
                    def = "dict -h dict.org";
                    rm = "rmtrash";
                    rmlist = "trash-list";
                    rmdir = "rmdirtrash";
                    unrm = "trash-restore";
                    xrm = "trash-empty";
                    clip = "xclip -r -selection c";
                };    
                initExtra = ''
                    autoload -U colors && colors
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

                    # RPROMPT="%{$fg[yellow]%}%t"
                    source $NIXOS_CONFIG/dotfiles/br.sh
                    eval $(thefuck --alias)

                    # precmd() { echo -ne '\e[4 q' }

                    bindkey "^[[1;3D" backward-word 
                    bindkey "^[[1;3C" forward-word

                    export ATUIN_NOBIND="true"
                    eval "$(atuin init zsh)"
                    bindkey '^[[1;5A' _atuin_up_search_widget
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
            atuin = {
                enable = true;
                enableZshIntegration = true;
                flags = [
                  "--disable-up-arrow"
                ];
            };
            gh = { enable = true; };
            zoxide = {
                enable = true;
                enableZshIntegration = true;
            };
        };

        xresources.properties = {
            "Xcursor.size" = 16;
            "Xcursor.theme" = "Adwaita";
        };
        home.stateVersion = "23.11";
        programs.home-manager.enable = true;
    };
}
