{ config, pkgs, ... }:

{
    home.packages = with pkgs; [
        (
            writeShellScriptBin "bulkcompile" ''
                cwd="$PWD"
                while IFS= read -r -d "" dir
                do
                    cd "$dir" || exit
                    $0 "$1" "$2"
                done <   <(find "$cwd" -mindepth 1 -maxdepth 1 -type d -print0)

                find "$cwd" -mindepth 1 -maxdepth 1 -name "*.$1" -exec "$2" {} \;
                cd "$cwd" || exit
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
                basewd=$PWD
                cd $PROJECTS
                for dir in */
                do
                    echo "\e[34m > Entering $dir... \e[0m"
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
                basewd=$PWD
                cd $PROJECTS
                for dir in */
                do
                    echo "Entering $dir..."
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
    ];
}
