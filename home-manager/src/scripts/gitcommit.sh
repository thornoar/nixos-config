#!/usr/bin/env bash

is_git_repo="$(git rev-parse --is-inside-work-tree 2> /dev/null)"

if [ "$is_git_repo" != "true" ]; then
    printf "\e[1;31m error:\e[0m Not a git repository.\n" # ]]
    exit 1
fi

root_dir="$(git rev-parse --show-toplevel)"
curdir="$PWD"

cd "$root_dir" || exit

echo -e "GET http://google.com HTTP/1.0\n\n" | nc google.com 80 > /dev/null 2>&1
online="$?"

if [ $online != 0 ]; then
    printf "| \e[34mThe internet connection is down.\e[0m\n" #]]
    while true; do
        read -r -e -p "| \e[35mWould you like to commit the changes without pushing? [yN]\e[0m " ans # ]]
        # (((
        case $ans in
            [Yy]* ) git add . && git commit -m "$1" && exit 0 ;;
            [Nn]* ) exit 0 ;;
            * ) printf "| \e[33mPlease answer [y]es or [n]o\e[0m\n" # ]]
        esac
    done
else
    git add .
    git commit -m "$1"
    git push
fi

cd "$curdir" || exit
