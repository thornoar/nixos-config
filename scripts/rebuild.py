#!/usr/bin/env python

import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("type")
parser.add_argument("-i", "--impure", action = "store_true", help = "use the corresponding flag in \"nixos-rebuild\"")
parser.add_argument("-d", "--default", action = "store_true", help = "do not use nix-output-manager to show build progress")
parser.add_argument("-n", "--nodiff", action = "store_true", help = "do not use nvd to diff the new generation with the old one")
parser.add_argument("-u", "--update", action = "store_true", help = "update the flake.lock file, saving the previous one")
parser.add_argument("-r", "--restore", action = "store_true", help = "restore the previous flake.lock file")
parser.add_argument("-b", "--reboot", action = "store_true", help = "reboot the system after the rebuild")
parser.add_argument("-c", "--command", type = str, default = "switch", help = "command to use with \"nixos-rebuild\". default is \"switch\"")
parser.add_argument("-s", "--specialisation", type = str, default = "auto", help = "the specialisation to switch to.")
parser.add_argument("-f", "--flake", type = str, default = os.environ["NIXOS_CONFIG"], help = "flake to use. default is \"$NIXOS_CONFIG\". a value of \"--\" will disable flakes")
parser.add_argument("-o", "--output", type = str, default = "auto", help = "flake output to use. default is \"master\"")
parser.add_argument("-e", "--extra", type = str, default = "", help = "extra options to pass to \"nixos-rebuild\"")
args = parser.parse_args()

command = ""
if (args.type == "home"):
    command = "home-manager"
elif (args.type == "system"):
    command = "sudo nixos-rebuild"
else:
    print("| \033[31mFailed to recognize command:\033[0m " ++ args.type) # ]]
    exit(1)

print("| \033[34mRebuild command is: \"\033[35m" + command + "\033[34m\".\033[0m") #]]]]

def call (str):
    if 0 != os.system(str):
        print("| \033[31mFailed to run complete.\033[0m") #]]
        # print(str)
        exit(1)
try:
    cwd = os.popen("pwd").read().strip()
    new_dir = (args.flake != "auto") and args.flake or os.environ["NIXOS_CONFIG"]
    print("| \033[34mChanging to flake directory: \"\033[35m" + new_dir + "\033[34m\".\033[0m") #]]]]
    os.chdir(new_dir)

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
                output = os.popen("hostname -f").read().strip()
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

    print("| \033[34mBuilding configuration...\033[0m") #]]

    old_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (args.extra != ""):
        args.extra = args.extra.replace(", ", " --")
        args.extra = "--" + args.extra

    spec_args = ""

    if (args.type == "system"):
        spec_args = "--specialisation "
        if (args.specialisation == "auto"):
            if (os.environ["SPECIALISATION"] != ""):
                spec_args += os.environ["SPECIALISATION"]
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
        call("sudo printf \"\033[33mAccess granted.\033[0m\\n\"") #]]

    call(command + " " + args.command + " " + spec_args + " " + args.extra + " " + flake_args + nom_args)

    new_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (not args.nodiff):
        if (args.type == "system"):
            call("nvd diff " + old_gen + " " + new_gen)

    os.chdir(cwd)

    if args.reboot:
        os.system("reboot")
except KeyboardInterrupt:
    print("\n| \033[31mInterrupted by user.\033[0m")
