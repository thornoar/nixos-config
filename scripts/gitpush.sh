#!/bin/sh
grep_colors="$GREP_COLORS"
printf "\e[34m> Pushing to remote repository (branch \e[33m%s\e[34m)...\e[0m\n" "$(git rev-parse --abbrev-ref HEAD)"
git remote update
if git status | grep -q "branch is up to date"; then
    git add . && git commit -m '--' && git push
else
    export GREP_COLORS="ms=1;91"
    git status | grep --color "behind"
    export GREP_COLORS="$grep_colors"
    printf "\e[34m> There are unpulled changes, please resolve all conflicts. Quitting...\e[0m\n"
fi
