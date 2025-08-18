#!/usr/bin/env bash

is_git_repo="$(git rev-parse --is-inside-work-tree 2> /dev/null)"

if [ "$is_git_repo" != "true" ]; then
    printf "\e[1;31m#\e[0m Not a git repository.\n" # ]]
    exit 1
fi

something_to_commit="$(git status --porcelain=v1 2>/dev/null | wc -l)"

if [ "$something_to_commit" -eq 0 ]; then
    printf "\e[1;31m#\e[0m Nothing to commit.\n" # ]]
    exit 1
fi

root_dir="$(git rev-parse --show-toplevel)"
curdir="$PWD"

message="$1"
if [ -z "$message" ]; then
    message="$(date +"%d %b %Y (%a): %H:%M")"
fi

cd "$root_dir" || exit

echo -e "GET http://google.com HTTP/1.0\n\n" | nc google.com 80 > /dev/null 2>&1
online="$?"

if [ $online -ne 0 ]; then
    printf "\e[1;34m#\e[0m The internet connection is down.\n" #]]
    while true; do
        read -r -e -p "\e[1;35m#\e[0m Would you like to commit the changes without pushing? [yN] " ans # ]]
        # (((
        case $ans in
            [Yy]* ) git add . && git commit -m "$message" && exit 0 ;;
            [Nn]* ) exit 0 ;;
            * ) printf "\e[1;33m#\e[0m Please answer [y]es or [n]o\e[0m\n" # ]]
        esac
    done
else
    git add .
    git commit -m "$message"
    git push
fi

cd "$curdir" || exit
