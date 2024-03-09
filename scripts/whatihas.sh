echo -e "\e[35m| List Installed User Packages |\e[0m"
# for package in $(<"$NIXOS_CONFIG/home-packages"); do
#     nix search nixpkgs#"$package" > /dev/null
# done
for package in $(<"$NIXOS_CONFIG/home-packages"); do
    nix search nixpkgs#"$package" | grep --color "$package"
done
