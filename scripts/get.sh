echo -e "\[35m| Installing Package \"$1\"|\[0m"
if grep -q $1 "$NIXOS_CONFIG/home-packages"; then
	echo -e "\[34m> Package already listed in configuration. Run \"sudo nixos-rebuild switch\" to install. Quitting...\[0m"
	exit 0
fi
echo -e "\[34m> Prepending \"$1\" to package list...\[0m"
echo $1 >> "$NIXOS_CONFIG/home-packages"
echo -e "\[34m> Rebuilding system configuration...\[0m"
srb