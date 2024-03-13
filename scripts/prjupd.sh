#!/usr/bin/env bash
cwd=$PWD
cd "$PROJECTS" || exit
for dir in */
do
    echo ""
    printf "\e[34m> Entering %s...\e[0m\n" "$dir"
    cd "$dir" || exit
    gitupd
    cd ..
done
cd "$cwd" || exit
