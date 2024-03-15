#!/usr/bin/env bash

printf "test question (y/n) "
read ans
if [ "$ans" != "${ans#[Yy]}" ]; then
    echo "yes"
else
    echo "no"
fi
# select yn in "Yes" "No"; do
#     case $yn in
#         Yes ) echo "yes";;
#         No ) echo "no";;
#     esac
# done
# printf "\e[33m> NixOS configuration folder already detected at \"\e[35m~/projects/nixos-config/\e[34m\". Do you want to delete it and reinstall? (y/n)\e[0m \n"
# select ync in "y" "n"; do
#     case $ync in
#         "y" )
#             printf "\e[34m> Your NixOS configuration will be moved to \"\e[35m~/projects/old-nixos-config/\e[34m\".\e[0m\n"
#             sudo mv "~/projects/nixos-config" "~/projects/old-nixos-config"
#         ;;
#         "n" )
#             printf "\e[31mEnjoy your old configuration. I couldn't care less.\e[0m\n"
#             exit 1
#         ;;
#     esac
# done
