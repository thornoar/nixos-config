echo -e "\e[35m| Update The System |\e[0m"
echo -e "\e[34m> Pushing git repository...\e[0m"
cwd=$PWD
cd $NIXOS_CONFIG
git add . && git commit -m '--' && git push
cd $cwd
echo -e "\e[34m> Building configuration...\e[0m"
sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master || exit 1
echo -e "\e[34m> Recompiling window manager...\e[0m"
killall xmobar
recompile_xmonad && xmonad --restart
