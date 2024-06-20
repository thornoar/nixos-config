#!/usr/bin/env python

import argparse
import subprocess
import os
import sys

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--simple", action = "store_true", help = "simply cat the package list file")
args = parser.parse_args()

if (args.simple):
    os.system("cat $NIXOS_CONFIG/home-manager/packages.txt")
    sys.exit(0)

def run_command (command):
    result = subprocess.run(command, capture_output = True, text = True)
    output = result.stdout.strip()
    return output

pacfile = open(os.environ["NIXOS_CONFIG"] + "/home-manager/packages.txt").readlines()

for package in pacfile:
    package = package.replace("\n", "")
    out = run_command(["nix", "search", "nixpkgs#"+package]).replace(package, "\033[1;33m"+package+"\033[0m") #]]
    print(out)
