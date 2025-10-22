#!/usr/bin/env bash

curdir=$(pwd)
cd "$HOME/projects" || exit
blkcmd dir "git status -s" -c
cancelled=0

function ask {
    read -r -p "? Commit & push all changes? (y/N) " answer
    answer=${answer:-Y}
    if [[ "$answer" =~ ^[Yy]$ ]]; then
        cd "$HOME/projects" || exit
        blkcmd dir "gitcommit" -c
    else
        cancelled=1
    fi
}

for dir in ./*; do
    (
        cd "$dir" || exit
        git diff-index --quiet HEAD || ask
        if [ $cancelled = 1 ]; then
            echo "> Cancelled."
            exit 0
        fi
    )
done

cd "$curdir" || exit
