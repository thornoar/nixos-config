#!/bin/sh
export CHECK_GIT=true
export COMMAND="switch"
export EXTRA_OPTIONS=""
export OUTPUT="master"

grep_colors="$GREP_COLORS"

while getopts "hpic:e:o:" flag; do
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
            echo "  -c [COMMAND]    COMMAND to use with \"nixos-rebuild\"; default is \"switch\""
            echo "  -e [OPTIONS]    Extra options to add to the \"nixos-rebuild\" command"
            echo "  -o [OUTPUT]     Flake output to use for system configuration; default is \"master\""
            exit 0
        ;;
        p)
            export CHECK_GIT=false
        ;;
        i)
            export EXTRA_OPTIONS="$EXTRA_OPTIONS --impure"
        ;;
        c)
            export COMMAND="$OPTARG"
        ;;
        e)
            export EXTRA_OPTIONS="$EXTRA_OPTIONS $OPTARG"
        ;;
        o)
            export OUTPUT="$OPTARG"
        ;;
        *)
            # printf "\e[31merror: \e[0minvalid flags.\n"
            exit 1
        ;;
    esac
done

printf "\e[35m| Update The System |\e[0m\n"

if $CHECK_GIT; then
    cwd=$PWD
    cd "$NIXOS_CONFIG" || exit
    if eval git rev-parse --is-inside-work-tree; then
        printf "\e[34m> Checking git repository on branch \e[33m$(git rev-parse --abbrev-ref HEAD)\e[34m...\e[0m\n"
        git remote update
        if git status | grep -q "branch is up to date"; then
            export GREP_COLORS="ms=1;94"
            git status | grep --color "branch is up to date"
            if git status | grep -q "working tree clean"; then
                git status | grep --color "working tree clean"
            else
                echo "Local changes:"
                git status -s
                printf "\e[34m> Pushing changes...\e[0m\n"
                git add . && git commit -m '--' && git push
            fi
            export GREP_COLORS=$grep_colors
        elif git status | grep -q "Changes not staged for commit"; then
            export GREP_COLORS="ms=1;91"
            git status | grep --color "behind"
            git status | grep --color "not staged"
            export GREP_COLORS=$grep_colors
            printf "\e[34m> Clash between local and remote branches, skipping...\e[0m\n"
        else
            export GREP_COLORS="ms=1;91"
            git status | grep --color "behind"
            export GREP_COLORS=$grep_colors
            printf "\e[34m> Pulling remote changes...\e[0m\n"
            git fetch && git pull
        fi
    fi
    cd $cwd
fi

exit

printf "\e[34m> Building configuration...\e[0m\n"
sudo nixos-rebuild $COMMAND $EXTRA_OPTIONS --flake $NIXOS_CONFIG/#master || exit 1
printf "\e[34m> Recompiling XMonad...\e[0m\n"

killall xmobar
recompile_xmonad && xmonad --restart
