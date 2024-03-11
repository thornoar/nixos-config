#!/usr/bin/env bash
cwd="$PWD"

while IFS= read -r -d "" dir
do
    printf "\e[34m> Entering $dir...\e[0m\n"
    cd "$dir" || exit 1
    $0 "$@"
done <   <(find "$cwd" -mindepth 1 -maxdepth 1 -type d -print0)

# for file in *.$1; do
#     printf "\e[34m> Executing \"$2 ''${@:3}\" on $PWD/$file... \e[0m\n"
#     "$2" "''${@:3}" "$file"
# done

find "$cwd" -mindepth 1 -maxdepth 1 -name "*.$1" -exec "${@:2}" {} \;
cd "$cwd" || exit 1
