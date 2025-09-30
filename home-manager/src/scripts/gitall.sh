#!/usr/bin/env bash

curdir=$(pwd)
cd "$HOME/projects" || exit
blkcmd dir "git status -s" -c

function ask {
    read -r -p "? Commit & push all changes? (y/N) " answer
    answer=${answer:-Y}
    if [[ "$answer" =~ ^[Yy]$ ]]; then
        cd "$HOME/projects" || exit
        blkcmd dir "gitcommit" -c
    else
        echo "> Cancelled."
    fi
}

for dir in ./*; do
    (
        cd "$dir" || exit
        git diff-index --quiet HEAD || ask
    )
done

cd "$curdir" || exit
