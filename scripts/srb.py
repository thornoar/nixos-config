#!/usr/bin/env python

import os
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-p", "--preserve", action = "store_true", help = "rebuild the system without pushing changes to git")
parser.add_argument("-c", "--command", type = str, default = "switch", help = "command to use with \"nixos-rebuild\". default is \"switch\"")
parser.add_argument("-f", "--flake", type = str, default = "$NIXOS_CONFIG", help = "flake to use. default is \"$NIXOS_CONFIG\". a value of \"--\" will disable flakes")
parser.add_argument("-o", "--output", type = str, default = "master", help = "flake output to use. default is \"master\"")
parser.add_argument("-i", "--impure", action = "store_true", help = "use the corresponding flag in \"nixos-rebuild\"")
parser.add_argument("-e", "--extra", type = str, default = "", help = "extra options to pass to \"nixos-rebuild\"")
parser.add_argument("-m", "--message", type = str, default = "system rebuild", help = "message to use with git. default is \"system rebuild\"")
args = parser.parse_args()

if (not args.preserve and args.flake != "--"):
    os.system("gitupd -t " + args.flake)

flakeopt = ""
if (args.flake != "--"):
    flakeopt = " --flake " + args.flake + "#" + args.output
if (args.impure):
    args.extra = "--impure " + args.extra

print("\033[34m> Building configuration...\033[0m") #]]
os.system("sudo nixos-rebuild " + args.command + " " + args.extra + flakeopt)
