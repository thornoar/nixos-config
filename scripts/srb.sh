check_git=true
command="switch"
extra_options=""

while getopts "hpic:e:" flag; do
    case $flag in
    h)
        echo "Usage: srb [OPTION]"
        echo "Rebuild NixOS system configuration and recompile window manager"
        echo "Example: srb -i -p"
        echo ""
        echo "Available options:"
        echo "  -h              Display this [h]elp message"
        echo "  -p              [P]reserve the local configuration; do not check for git changes"
        echo "  -i              Use the --[i]mpure flag with \"nixos-rebuild\". Necessary when configuration contains absolute paths"
        echo "  -c [COMMAND]    Command to use with \"nixos-rebuild\"; default is \"switch\""
        echo "  -e [OPTIONS]    Extra options to add to the \"nixos-rebuild\" command"
        exit 0
    ;;
    p)
        check_git=false
    ;;
    i)
        extra_options="$extra_options --impure"
    ;;
    c)
        command=$OPTARG
    ;;
    e)
        extra_options="$extra_options $OPTARG"
    ;;
    esac
done

echo -e "\e[35m| Update The System |\e[0m"

if check_git; then
    echo -e "\e[34m> Checking git repository...\e[0m"
    
    cwd=$PWD
    cd $NIXOS_CONFIG
    git add . && git commit -m '--' && git push
    cd $cwd
fi

echo -e "\e[34m> Building configuration...\e[0m"
sudo nixos-rebuild switch --impure --flake $NIXOS_CONFIG/#master || exit 1
echo -e "\e[34m> Recompiling window manager...\e[0m"
killall xmobar
recompile_xmonad && xmonad --restart
