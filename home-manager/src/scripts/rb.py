#!/usr/bin/env python

import os
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--impure", action = "store_true", help = "use the corresponding flag in \"nixos-rebuild\"")
parser.add_argument("-d", "--default", action = "store_true", help = "do not use nix-output-manager to show build progress")
parser.add_argument("-n", "--nodiff", action = "store_true", help = "do not use nvd to diff the new generation with the old one")
parser.add_argument("-u", "--update", action = "store_true", help = "update the flake.lock file, saving the previous one")
parser.add_argument("-r", "--restore", action = "store_true", help = "restore the previous flake.lock file")
parser.add_argument("-b", "--reboot", action = "store_true", help = "reboot the system after the rebuild")
parser.add_argument("-g", "--git", action = "store_true", help = "use git to syncronize the flake directory with a remote repository")
parser.add_argument("-t", "--type", type = str, default = "system", help = "system or home")
parser.add_argument("-c", "--command", type = str, default = "switch", help = "command to use with \"nixos-rebuild\". default is \"switch\"")
parser.add_argument("-s", "--specialisation", type = str, default = "auto", help = "the specialisation to switch to.")
parser.add_argument("-f", "--flake", type = str, default = "auto", help = "flake to use. default is \"$NIXOS_CONFIG\". a value of \"--\" will disable flakes")
parser.add_argument("-o", "--output", type = str, default = "auto", help = "flake output to use. default is \"master\"")
parser.add_argument("-e", "--extra", type = str, default = "", help = "extra options to pass to \"nixos-rebuild\"")
args = parser.parse_args()

def call (str):
    if 0 != os.system(str):
        print("| \033[31mFailed to complete.\033[0m") #]]
        exit(1)

if (args.type == "both"):
    call("rb system " + " ".join(sys.argv[2:]))
    call("rb home " + " ".join(sys.argv[2:]))
    exit(0)

command = ""
if (args.type == "home"):
    command = "home-manager"
elif (args.type == "system"):
    command = "sudo nixos-rebuild"
else:
    print("| \033[31mFailed to recognize command:\033[0m " + args.type) # ]]
    exit(1)

print("| \033[34mRebuild command is: \"\033[35m" + command + "\033[34m\".\033[0m") #]]]]

try:
    cwd = os.popen("pwd").read().strip()
    new_dir = (args.flake != "auto") and args.flake or ("NIXOS_CONFIG" in os.environ and os.environ["NIXOS_CONFIG"] or os.environ["HOME"])
    print("| \033[34mChanging to flake directory: \"\033[35m" + new_dir + "\033[34m\".\033[0m") #]]]]
    os.chdir(new_dir)

    behind_remote = False

    if args.git:
        print("| \033[34mUpdating the remote repository.\033[0m") #]]
        call("git remote update")
        if "branch is behind" in os.popen("git status").read().strip():
            print("| \033[34mLocal branch is behind remote .\033[0m") #]]
            behind_remote = True
        if "not staged for commit" in os.popen("git status").read().strip():
            print("| \033[34mCommitting local changes.\033[0m") #]]
            call("git add -A")
            call("git commit -m \"automatic commit\"")
            call("git pull --no-rebase --no-edit")
            call("git push")
        elif behind_remote:
            print("| \033[34mWorking tree clean, pulling changes.\033[0m") #]]
            call("git fetch && git pull")

    if (args.restore and os.path.exists("./flake.lock.bak")):
        print("| \033[34mRestoring previous flake.lock file...\033[0m") #]]
        call("mv ./flake.lock.bak ./flake.lock")
    elif (args.update):
        print("| \033[34mUpdating flake.lock file...\033[0m") #]]
        call("cp ./flake.lock ./flake.lock.bak")
        call("nix flake update")

    flake_args = ""
    if (args.flake != "--"):
        output = args.output
        if (output == "auto"):
            if (args.type == "system"):
                output = os.popen("hostname").read().strip()
            if (args.type == "home"):
                output = os.environ["USER"]
        flake_args = "--flake .#" + output
        if (args.impure):
            flake_args += " --impure"
        print("| \033[34mFlake arguments: \"\033[35m" + flake_args + "\033[34m\".\033[0m") #]]]]

    nom_args = ""
    if (not args.default):
        if (args.type == "system"):
            nom_args = " --log-format internal-json |& nom --json"
        if (args.type == "home"):
            nom_args = " |& nom"

    old_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (args.extra != ""):
        args.extra = args.extra.replace(", ", " --")
        args.extra = "--" + args.extra

    spec_args = ""

    if (args.type == "system"):
        spec_args = "--specialisation "
        if (args.specialisation == "auto"):
            env = os.environ["SPECIALISATION"]
            if (env == "default"):
                spec_args = ""
            elif (env != ""):
                spec_args += env
            else:
                if (os.environ["XDG_SESSION_TYPE"] == "wayland"):
                    spec_args += "hyprland"
                elif (os.environ["XDG_SESSION_TYPE"] == "none+xmonad"):
                    spec_args += "xmonad"
                else:
                    spec_args = ""
        else:
            spec_args += args.specialisation
        print("| \033[34mSpecialisation arguments are: \"\033[35m" + spec_args + "\033[34m\".\033[0m") #]]]]
        call("sudo printf \"| \033[33mAccess granted.\033[0m\\n\"") #]]

    print("| \033[34mBuilding configuration...\033[0m") #]]

    call(command + " " + args.command + " " + spec_args + " " + args.extra + " " + flake_args + nom_args)

    new_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (not args.nodiff):
        if (args.type == "system"):
            call("nvd diff " + old_gen + " " + new_gen)
        else:
            call("hmd --auto")

    os.chdir(cwd)

    if args.reboot:
        os.system("reboot")
except KeyboardInterrupt:
    print("\n| \033[31mInterrupted by user.\033[0m")
