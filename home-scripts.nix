{ config, pkgs, ... }:

{
    home.packages = with pkgs; [
        (
            writeShellScriptBin "blkcmd" ''
                cwd="$PWD"
                while IFS= read -r -d "" dir
                do
                    echo -e "\e[34m> Entering $dir... \e[0m"
                    cd "$dir" || exit 1
                    $0 "$@"
                done <   <(find "$cwd" -mindepth 1 -maxdepth 1 -type d -print0)

                # for file in *.$1; do
                #     echo -e "\e[34m> Executing \"$2 ''${@:3}\" on $PWD/$file... \e[0m"
                #     "$2" "''${@:3}" "$file"
                # done

                find "$cwd" -mindepth 1 -maxdepth 1 -name "*.$1" -exec "''${@:2}" {} \;
                cd "$cwd" || exit 1
            ''
        )
        (
            writeShellScriptBin "pdfviewall" ''
                for file in "$PWD"/*; do
                    if [[ $file == *.pdf ]]
                    then
                        nohup zathura "$file"&
                    fi
                done;
            ''
        )
        (
            writeShellScriptBin "chlang" ''
                setxkbmap -layout us,ru,de
                xkb-switch -s "$1"
                if [ "$LAPTOP" ]; then
                    xmodmap "$HOME"/.Xmodmap
                fi
            ''
        )
        (
            writeShellScriptBin "monitorrent" ''
                alacritty -e watch --interval 0.5 "transmission-remote -l"
            ''
        )
        (
            writeShellScriptBin "inc_bright" ''
                brightfile=""
                if [ "$PCTYPE" = "laptop" ]; then
                    brightfile="/sys/class/backlight/intel_backlight/brightness"
                    bc <<< "$1 + $(<$brightfile)" > "$brightfile"
                fi
            ''
        )
        (
            writeShellScriptBin "gitpush" "git add . && git commit -m '--' && git push"
        )
        (
            writeShellScriptBin "gitpull" "git fetch && git pull"
        )
        (
            writeShellScriptBin "fgps" ''
                echo -e "\e[32m [bulk git push script] \e[0m"
                basewd=$PWD
                cd $PROJECTS
                for dir in */
                do
                    echo ""
                    echo -e "\e[34m> Entering $dir... \e[0m"
                    cd $dir
                    if [ -d .git ]; then
                        git add . && git commit -m "--" && git push
                    else
                        echo "Not a git repository, skipping..."
                    fi
                    cd ..
                done
                cd "$basewd"
            ''
        )
        (
            writeShellScriptBin "fgpl" ''
                echo -e "\e[32m [bulk git pull script] \e[0m"
                basewd=$PWD
                cd $PROJECTS
                for dir in */
                do
                    echo ""
                    echo -e "\e[34m> Entering $dir... \e[0m"
                    cd $dir
                    if [ -d .git ]; then
                        git fetch && git pull
                    else
                        echo "Not a git repository, skipping..."
                    fi
                    cd ..
                done
                cd "$basewd"
            ''
        )
        (
            writeShellScriptBin "run" "nix run nixpkgs#$1 -- \${@:2}"
        )
        (
            writeShellScriptBin "call" "$1 $(broot .) \${@:2}"
        )
        (
            writeShellScriptBin "recompile_xmonad" ''
                echo "Custom XMonad recompilation"
                tmp=$(mktemp -d)
                touch $tmp/xmessage
                chmod +x $tmp/xmessage
                # sed_arg="'s/XMESSAGE=.*/XMESSAGE=\'$tmp\/xmessage\'/g'"
                cat $(which xmonad) | sed "s/XMESSAGE=.*/XMESSAGE=\'\.\/xmessage\'/g" > $tmp/xmonad
                # cp $(which xmonad) > $tmp/xmonad
                # edit ./xmonad to change XMONAD_MESSAGE to set it to $tmp/xmonad
                chmod +x $tmp/xmonad
                # now we can run:
                $tmp/xmonad --recompile
            ''
        )
        (
            writeShellScriptBin "rb" ''
                sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/$1
            ''
        )
        (
            writeShellScriptBin "hrb" ''
                home-manager switch --impure --flake $NIXOS_CONFIG/
                recompile_xmonad && xmonad --restart
            ''
        )
        (
            writeShellScriptBin "srb" ''
                sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master || exit 1
                recompile_xmonad && xmonad --restart
            ''
        )
        (
            writeShellScriptBin "gcollect" ''
                nix-collect-garbage --delete-old && sudo nix-collect-garbage --delete-old
            ''
        )
        # (
        #     writeShellScriptBin "frb" ''
        #         sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master || exit 1
        #         home-manager switch --impure --flake $NIXOS_CONFIG/
        #         recompile_xmonad && xmonad --restart
        #     ''
        # )
    ];
}
