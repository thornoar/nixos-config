#!/usr/bin/env bash
killall xmobar
tmp=$(mktemp -d)
touch "$tmp/xmessage"
chmod +x "$tmp/xmessage"
sed "s/XMESSAGE=.*/XMESSAGE=\'\.\/xmessage\'/g" "$(which xmonad)" > "$tmp"/xmonad
chmod +x "$tmp/xmonad"
"$tmp/xmonad" --recompile
xmonad --restart
