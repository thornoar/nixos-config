#!/usr/bin/env bash
cwd=$PWD
cd "$PROJECTS" || exit
for dir in */
do
    echo ""
    cd "$dir" || exit
    printf "\e[34m> Entered \e[35m%s\e[0m\n" "$dir"
    gitupd
    cd ..
done
cd "$cwd" || exit
