tmp=$(mktemp -d)
touch $tmp/xmessage
chmod +x $tmp/xmessage
# sed_arg="'s/XMESSAGE=.*/XMESSAGE=\'$tmp\/xmessage\'/g'"
cat $(which xmonad) | sed "s/XMESSAGE=.*/XMESSAGE=\'\.\/xmessage\'/g" > $tmp/xmonad
# cp $(which xmonad) > $tmp/xmonad
# edit ./xmonad to change XMONAD_MESSAGE to set it to $tmp/xmonad
chmod +x $tmp/xmonad
# now we can run:
$tmp/xmonad --recompile
