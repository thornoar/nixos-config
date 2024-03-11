#!/usr/bin/env bash
check_git=true
command="switch"
extra_options=""
output="master"

grep_colors="$GREP_COLORS"

while getopts "hpic:e:o:m:" flag; do
    case $flag in
        h)
            echo "Usage: srb [OPTION]"
            echo "Rebuild NixOS system configuration"
            echo "Example: srb -i -p"
            echo ""
            echo "Available options:"
            echo "  -h              Display this [h]elp message"
            echo "  -p              [P]reserve the local configuration; do not check for git changes"
            echo "  -i              Use the --[i]mpure flag with \"nixos-rebuild\". Necessary when configuration contains absolute paths"
            echo "  -c [COMMAND]    Command to use with \"nixos-rebuild\"; default is \"switch\""
            echo "  -e [OPTIONS]    Extra options to add to the \"nixos-rebuild\" command"
            echo "  -o [OUTPUT]     Flake output to use for system configuration; default is \"master\""
            echo "  -m [OPTION]     Specify window manager in use"
            exit 0
        ;;
        p)
            check_git=false
        ;;
        i)
            extra_options="$extra_options --impure"
        ;;
        c)
            command="$OPTARG"
        ;;
        e)
            extra_options="$extra_options $OPTARG"
        ;;
        o)
            output="$OPTARG"
        ;;
        m)
            output="$OPTARG"
        ;;
        *) exit 1
        ;;
    esac
done

# printf "\e[35m| Update The System |\e[0m\n"

if $check_git; then
    cwd="$PWD"
    cd "$NIXOS_CONFIG" || exit
    gitupd -m "system update"
    cd "$cwd" || exit
fi

printf "\e[34m> Building configuration...\e[0m\n"
sudo nixos-rebuild $command $extra_options --flake $NIXOS_CONFIG/#$output || exit 1
