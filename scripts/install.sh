echo -e "\e[35m| Install Packages |\e[0m"
if grep -q $1 "$NIXOS_CONFIG/home-packages"; then
	echo -e "\e[34m> Package already listed in configuration. Run \"sudo nixos-rebuild switch\" to install. Quitting...\e[0m"
	exit 0
fi
nix search nixpkgs#$1 || exit 1
echo -e "\e[34m> Prepending \"\e[0m\e[35m$1\e[0m\e[34m\" to package list...\e[0m"
echo "$1" >> "$NIXOS_CONFIG/home-packages"
srb
