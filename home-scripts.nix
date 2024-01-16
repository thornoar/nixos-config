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

                find "$cwd" -mindepth 1 -maxdepth 1 -name "*.$1" -exec $2 {} \;
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
                if [ $LAPTOP ]; then
                    xmodmap $HOME/.Xmodmap
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
                if [ $PCTYPE = "laptop" ]; then
                    brightfile="/sys/class/backlight/intel_backlight/brightness"
                    bc <<< "$1 + $(<$brightfile)" > "$brightfile"
                fi
            ''
        )
        (
            writeShellScriptBin "gitpush" ''
                nohup git status || (echo "not a git repository!" && exit 1)
            ''
        )
        (
            writeShellScriptBin "run" "nix run nixpkgs#$1 -- \${@:2}"
        )
    ];
}
