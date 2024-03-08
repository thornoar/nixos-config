echo -e "\e[35m| Collect Garbage |\e[0m"
echo -e "\e[34m> Collecting on the user level...\e[0m"
nix-collect-garbage --delete-old
echo -e "\e[34m> Collecting on the root level...\e[0m"
sudo nix-collect-garbage --delete-old
