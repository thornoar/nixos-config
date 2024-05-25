#!/usr/bin/env bash

killall xmobar

while getopts "bx" option; do
    case $option in
        b) xmobar& ;;
        x) 
            tmp=$(mktemp -d)
            touch "$tmp/xmessage"
            chmod +x "$tmp/xmessage"
            sed "s/XMESSAGE=.*/XMESSAGE=\'\.\/xmessage\'/g" "$(which xmonad)" > "$tmp"/xmonad
            chmod +x "$tmp/xmonad"
            "$tmp/xmonad" --recompile
            xmonad --restart
        ;;
        \?) printf "error: invalid option\n" ;;
    esac
done

