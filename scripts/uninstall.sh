echo -e "\[35m| Unistalling Package \"$1\"|\[0m"
if grep -q $1 "$NIXOS_CONFIG/home-packages"; then
    echo -e "\[34m> Removing \"$1\" from package list...\[0m"
    awk "!/$1/" "$NIXOS_CONFIG/home-packages" > temp && mv temp "$NIXOS_CONFIG/home-packages"
    echo -e "\[34m> Rebuilding system configuration...\[0m"
    srb
else
	echo -e "\[34m> Package already removed from configuration. Run \"sudo nixos-rebuild switch to uninstall. Quitting...\"\[0m"
	exit 0
fi
