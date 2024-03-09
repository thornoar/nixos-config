#!/bin/sh
printf "\e[35m| Pull All Git Repositories |\e[0m\n"
basewd=$PWD
cd "$PROJECTS" || exit
for dir in */
do
    echo ""
    printf "\e[34m> Entering $dir...\e[0m\n"
    cd "$dir" || exit
    if [ -d .git ]; then
        git fetch && git pull
    else
        printf "\e[33mNot a git repository, skipping...\e[0m\n"
    fi
    cd ..
done
cd "$basewd" || exit
