#!/usr/bin/env bash

curdir=$(pwd)
cd "$HOME/projects" || exit
blkcmd dir "git status -s" -c
dirty=0

for dir in ./*; do
    cd "$dir" || exit
    if git diff-index --quiet HEAD; then
        dirty=1
    fi
    cd .. || exit
done

if [ $dirty = 1 ]; then
    read -r -p "? Commit & push all changes? (y/N) " answer
    answer=${answer:-Y}
    if [[ "$answer" =~ ^[Yy]$ ]]; then
        cd "$HOME/projects" || exit
        blkcmd dir "gitcommit" -c
    fi
fi

cd "$curdir" || exit
