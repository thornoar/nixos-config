echo -e "\e[35m| Installing Package \"$1\"|\e[0m"
if grep -q $1 "$NIXOS_CONFIG/home-packages"; then
	echo -e "\e[34m> Package already listed in configuration. Run \"sudo nixos-rebuild switch\" to install. Quitting...\e[0m"
	exit 0
fi
echo -e "\e[34m> Prepending \"$1\" to package list...\e[0m"
echo "$1" >> "$NIXOS_CONFIG/home-packages"
echo -e "\e[34m> Rebuilding system configuration...\e[0m"
srb
