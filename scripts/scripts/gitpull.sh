#!/bin/sh
printf "\e[34m> Pulling from remote repository...\e[0m\n"
git fetch && git pull
