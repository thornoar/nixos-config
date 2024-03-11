#!/usr/bin/env bash

message="--"

while getopts "hm:" flag; do
    case $flag in
        h)
            echo "Usage: gitupd [OPTIONS]"
            echo "Syncroize local git tree with remote repository"
            echo "Example: gitupd -m \"test commit\""
            echo ""
            echo "Available options:"
            echo "  -h              Display this [h]elp message"
            echo "  -m [MESSAGE]    Specify commit [m]essage for pushing; by default, the blank \"--\" is used"
            exit 0
        ;;
        m)
            message="$OPTARG"
        ;;
        *)
            exit 1
        ;;
    esac
done

if $(git rev-parse --is-inside-work-tree); then
    printf "\e[34m> Checking git repository on branch \e[33m%s\e[34m...\e[0m\n" "$(git rev-parse --abbrev-ref HEAD)"
    git remote update
    status=$(git status)
    if git status | grep -q "branch is up to date"; then
        export GREP_COLORS="ms=1;94"
        echo "$status" | grep --color "branch is up to date"
        if echo "$status" | grep -q "working tree clean"; then
            echo "$status" | grep --color "working tree clean"
        else
            printf "\e[34m> Local changes:\e[0m\n"
            git status -s
            printf "\e[34m> Pushing changes...\e[0m\n"
            git add . && git commit -m "$message" && git push
        fi
        export GREP_COLORS="$grep_colors"
    elif echo "$status" | grep -q "Changes not staged for commit"; then
        export GREP_COLORS="ms=1;91"
        echo "$status" | grep --color "behind"
        echo "$status" | grep --color "not staged"
        export GREP_COLORS="$grep_colors"
        printf "\e[31mfailure:\e[0m Clash between local and remote changes.\n"
    else
        export GREP_COLORS="ms=1;94"
        echo "$status" | grep --color "behind"
        export GREP_COLORS="$grep_colors"
        printf "\e[34m> Pulling remote changes...\e[0m\n"
        git fetch && git pull
    fi
else
    printf "\e[31mfailure:\e[0m Not a git repository.\n"
    exit 1
fi
