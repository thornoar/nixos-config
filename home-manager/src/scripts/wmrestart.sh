#!/usr/bin/env bash

killall xmobar

while getopts "bx" option; do
    case $option in
        b)
            nohup killall xmobar > /dev/null 2>&1
            nohup xmobar > /dev/null 2>&1 &
        ;;
        x) 
            tmp=$(mktemp -d)
            touch "$tmp/xmessage"
            chmod +x "$tmp/xmessage"
            sed "s/XMESSAGE=.*/XMESSAGE=\'\.\/xmessage\'/g" "$(which xmonad)" > "$tmp"/xmonad
            chmod +x "$tmp/xmonad"
            "$tmp/xmonad" --recompile
            nohup killall xmobar > /dev/null 2>&1
            xmonad --restart
        ;;
        *) printf "\e[1;31merror:\e[0m Invalid option: %s\n" "$option"; exit 1 ;;
    esac
done
