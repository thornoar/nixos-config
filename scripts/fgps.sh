#!/bin/sh
grep_colors="$GREP_COLORS"
printf "\e[35m| Push All Git Repositories |\e[0m\n"
basewd=$PWD
cd "$PROJECTS" || exit
for dir in */
do
    echo ""
    printf "\e[34m> Entering $dir...\e[0m\n"
    cd "$dir" || exit
    if [ -d .git ]; then
        printf "\e[34m> Pushing to remote repository (\e[33m%s\e[34m branch)...\e[0m\n" "$(git rev-parse --abbrev-ref HEAD)"
        git remote update
        if git status | grep -q "branch is up to date"; then
            export GREP_COLORS="ms=1;94"
            git status | grep --color "branch is up to date"
            export GREP_COLORS="$grep_colors"
            git add . && git commit -m '--' && git push
        else
            export GREP_COLORS="ms=1;91"
            git status | grep --color "behind"
            export GREP_COLORS="$grep_colors"
            printf "\e[34m> There are unpulled changes, please resolve all conflicts. Quitting...\e[0m\n"
        fi
    else
        printf "\e[33mNot a git repository, skipping...\e[0m\n"
    fi
    cd ..
done
cd "$basewd" || exit
