#!/usr/bin/env bash

clip=1
public=""
hidden=1

while getopts "cp:h" option; do
    case $option in
        c) clip=0 ;;
        p) public="$OPTARG" ;;
        h) hidden=0 ;;
        *) printf "\e[1;31error:\e[0m Invalid option: %s\n" "$option"; exit 1 ;;
    esac
done

if [ "$public" = "" ]; then
    public=$1
fi

if [ $hidden -eq 1 ]; then
    printf "enter private choice key: "; read -r choice
    printf "enter private shuffle key: "; read -r shuffle
else
    printf "enter private choice key: "; read -r -s choice; printf "\n"
    printf "enter private shuffle key: "; read -r -s shuffle; printf "\n"
fi

if [ $clip -eq 0 ]; then
    pshash "$public" "$choice" "$shuffle" | xclip -r -selection c
else
    pshash "$public" "$choice" "$shuffle"
fi
