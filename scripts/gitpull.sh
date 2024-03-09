#!/bin/sh
# printf "\e[34m> Pulling from remote repository...\e[0m\n"
# git fetch && git pull
grep_colors="$GREP_COLORS"
if $(git rev-parse --is-inside-work-tree); then
    printf "\e[34m> Pulling from remote repository (\e[33m%s\e[34m branch)...\e[0m\n" "$(git rev-parse --abbrev-ref HEAD)"
    git remote update
    if git status | grep -q "Changes not staged for commit"; then
        export GREP_COLORS="ms=1;91"
        git status | grep --color "not staged"
        export GREP_COLORS="$grep_colors"
        printf "\e[34m> There are unstaged local changes. Quitting...\e[0m\n"
        exit 1;
    else
        export GREP_COLORS="ms=1;94"
        git status | grep --color "working tree clean"
        export GREP_COLORS="$grep_colors"
        git fetch && git pull
    fi
fi
