echo -e "\e[35m| Unistalling Package \"$1\"|\e[0m"
if grep -q $1 "$NIXOS_CONFIG/home-packages"; then
    echo -e "\e[34m> Removing \"$1\" from package list...\e[0m"
    awk "!/$1/" "$NIXOS_CONFIG/home-packages" > temp && mv temp "$NIXOS_CONFIG/home-packages"
    srb
else
	echo -e "\e[34m> Package already removed from configuration. Run \"sudo nixos-rebuild switch\" to uninstall. Quitting...\"\e[0m"
	exit 0
fi
