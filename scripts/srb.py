#!/usr/bin/env python

import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--impure", action = "store_true", help = "use the corresponding flag in \"nixos-rebuild\"")
parser.add_argument("-s", "--short", action = "store_true", help = "do not use nix-output-manager to show build progress")
parser.add_argument("-n", "--nodiff", action = "store_true", help = "do not use nvd to diff the new generation with the old one")
parser.add_argument("-u", "--update", action = "store_true", help = "update the flake.lock file, saving the previous one")
parser.add_argument("-r", "--restore", action = "store_true", help = "restore the previous flake.lock file")
parser.add_argument("-b", "--reboot", action = "store_true", help = "reboot the system after the rebuild")
parser.add_argument("-c", "--command", type = str, default = "switch", help = "command to use with \"nixos-rebuild\". default is \"switch\"")
parser.add_argument("-f", "--flake", type = str, default = os.environ["NIXOS_CONFIG"], help = "flake to use. default is \"$NIXOS_CONFIG\". a value of \"--\" will disable flakes")
parser.add_argument("-o", "--output", type = str, default = "master", help = "flake output to use. default is \"master\"")
parser.add_argument("-e", "--extra", type = str, default = "", help = "extra options to pass to \"nixos-rebuild\"")
args = parser.parse_args()

def call (str):
    if 0 != os.system(str):
        print("| \033[31mFailed to complete.\033[0m") #]]
        exit(1)
try:
    cwd = os.popen("pwd").read().strip()
    os.chdir((args.flake != "--") and args.flake or os.environ["NIXOS_CONFIG"])

    if (args.restore and os.path.exists("./flake.lock.bak")):
        print("| \033[34mRestoring previous flake.lock file...\033[0m") #]]
        call("mv ./flake.lock.bak ./flake.lock")
    elif (args.update):
        print("| \033[34mUpdating flake.lock file...\033[0m") #]]
        call("cp ./flake.lock ./flake.lock.bak")
        call("nix flake update")

    flakeopt = ""
    if (args.flake != "--"):
        flakeopt = " --flake .#" + args.output
        if (args.impure):
            flakeopt += " --impure"

    log_format = ("" if args.short else " --log-format internal-json |& nom --json")

    print("| \033[34mBuilding configuration...\033[0m") #]]

    old_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (args.extra != ""):
        args.extra = args.extra.replace(", ", " --")
        args.extra = "--" + args.extra

    call("sudo printf \"\033[33mAccess granted.\033[0m\\n\"") #]]
    call("sudo nixos-rebuild " + args.command + " " + args.extra + flakeopt + log_format)

    new_gen = os.popen("readlink -f /run/current-system").read().strip()

    if (not args.nodiff):
        call("nvd diff " + old_gen + " " + new_gen)

    os.chdir(cwd)

    if args.reboot:
        os.system("reboot")
except KeyboardInterrupt:
    print("\n| \033[31mInterrupted by user.\033[0m")
