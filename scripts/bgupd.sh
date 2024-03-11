#!/usr/bin/env bash
grep_colors="$GREP_COLORS"
printf "\e[35m| Push All Git Repositories |\e[0m\n"
basewd=$PWD
cd "$PROJECTS" || exit
for dir in */
do
    echo ""
    printf "\e[34m> Entering $dir...\e[0m\n"
    cd "$dir" || exit
    gitupd
    cd ..
done
cd "$basewd" || exit
