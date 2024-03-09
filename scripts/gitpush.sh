#!/bin/sh
printf "\e[34m> Pushing to remote repository...\e[0m\n"
git add . && git commit -m '--' && git push
