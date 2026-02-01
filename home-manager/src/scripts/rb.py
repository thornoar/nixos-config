#!/usr/bin/env python

import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--impure", action = "store_true", help = "use the corresponding flag in \"nixos-rebuild\"")
parser.add_argument("-d", "--default", action = "store_true", help = "do not use nix-output-manager to show build progress")
parser.add_argument("-n", "--nodiff", action = "store_true", help = "do not use nvd to diff the new generation with the old one")
parser.add_argument("-u", "--update", action = "store_true", help = "update the flake.lock file, saving the previous one")
parser.add_argument("-r", "--restore", action = "store_true", help = "restore the previous flake.lock file")
parser.add_argument("-b", "--reboot", action = "store_true", help = "reboot the system after the rebuild")
parser.add_argument("-g", "--git", action = "store_true", help = "use git to syncronize the flake directory with a remote repository")
parser.add_argument("-G", "--onlygit", action = "store_true", help = "just commit changes, without rebuilding")
parser.add_argument("-a", "--alert", action = "store_true", help = "send a notification after rebuilding")
parser.add_argument("-c", "--command", type = str, default = "switch", help = "command to use with \"nixos-rebuild\". default is \"switch\"")
parser.add_argument("-s", "--specialisation", type = str, default = "auto", help = "the specialisation to switch to.")
parser.add_argument("-f", "--flake", type = str, default = "auto", help = "flake to use. default is \"$NIXOS_CONFIG\". a value of \"--\" will disable flakes")
parser.add_argument("-o", "--output", type = str, default = "auto", help = "flake output to use. default is \"master\"")
parser.add_argument("-e", "--extra", type = str, default = "", help = "extra options to pass to \"nixos-rebuild\"")
args = parser.parse_args()

CRED = "\033[31m"#]
CEND = "\033[0m"#]

def call (str):
    if 0 != os.system(str):
        print(CRED + "Failed to complete." + CEND)
        exit(1)

command = "sudo nixos-rebuild"

try:
    cwd = os.popen("pwd").read().strip()
    new_dir = (args.flake != "auto") and args.flake or ("NIXOS_CONFIG" in os.environ and os.environ["NIXOS_CONFIG"] or os.environ["HOME"])
    os.chdir(new_dir)

    behind_remote = False

    if args.git or args.onlygit:
        call("git remote update")
        status = os.popen("git status").read().strip()
        if "branch is behind" in status:
            print("> Local branch is behind remote.") #]]
            behind_remote = True
        if ("not staged for commit" in status) or "new file" in status:
            call("git add -A")
            call("git commit -m \"$(date +'%d %b %Y (%a): %H:%M')\"")
            call("git pull --no-rebase --no-edit")
            call("git push")
        elif behind_remote:
            print("> Working tree clean, pulling changes.") #]]
            call("git fetch && git pull")

    if args.onlygit:
        exit(0)

    if (args.restore and os.path.exists("./flake.lock.bak")):
        print("> Restoring previous flake.lock file...") #]]
        call("cp ./flake.lock.bak ./flake.lock")
    elif (args.update):
        print("> Updating flake.lock file...") #]]
        call("cp ./flake.lock ./flake.lock.bak")
        call("nix flake update")

    flake_args = ""
    if (args.flake != "--"):
        output = args.output
        if (output == "auto"):
            output = os.popen("hostname").read().strip()
        flake_args = "--flake .#" + output
        if (args.impure):
            flake_args += " --impure"

    nom_args = ""
    if (not args.default):
        nom_args = " --log-format internal-json |& nom --json"

    old_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (args.extra != ""):
        args.extra = args.extra.replace(", ", " --")
        args.extra = "--" + args.extra

    spec_args = ""
    # if (args.specialisation != "none" and os.environ["SPECIALISATION_ENABLE"] != "0"):
    #     spec_args = "--specialisation "
    #     if (args.specialisation == "auto"):
    #         env = os.environ["SPECIALISATION"]
    #         if (env == "default"):
    #             spec_args = ""
    #         elif (env != ""):
    #             spec_args += env
    #         else:
    #             if (os.environ["XDG_SESSION_TYPE"] == "wayland"):
    #                 spec_args += "hyprland"
    #             elif (os.environ["XDG_SESSION_TYPE"] == "none+xmonad"):
    #                 spec_args += "xmonad"
    #             else:
    #                 spec_args = ""
    #     else:
    #         spec_args += args.specialisation

    call("sudo echo -n") # ]]

    call(command + " " + args.command + " " + spec_args + " " + args.extra + " " + flake_args + nom_args)

    new_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (not args.nodiff):
        call("nvd diff " + old_gen + " " + new_gen)

    os.chdir(cwd)

    if args.alert:
        call("notify-send \"NixOS rebuild\" \"finished system upgrade\"")

    if args.reboot:
        os.system("reboot")
except KeyboardInterrupt:
    print(CRED + "Interrupted by user." + CEND)
