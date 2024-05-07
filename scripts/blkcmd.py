#!/usr/bin/env python

import os
import sys
import fnmatch
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-r", "--recursive", action = "store_true", help = "search for files recursively in current directory")
parser.add_argument("filetype", type = str)
parser.add_argument("command", type = str)
args = parser.parse_args()

CRED = "\033[31m"#]
CGREEN = "\033[32m"#]
CPURPLE = "\033[35m"#]
CBLUE = "\033[34m"#]
CEND = "\033[0m"#]

cwd = os.getcwd()

for root, dirnames, filenames in os.walk('.'):
    os.chdir(root)
    if (not args.recursive and root != "."):
        continue
    for filename in fnmatch.filter(filenames, "*."+args.filetype):
        head_tail = os.path.split(filename)
        file = root + "/" + filename
        print(CBLUE + "> Executing \"" + CGREEN + args.command + CBLUE + "\" on " + CPURPLE + file + CBLUE + "..." + CEND)
        os.system("parsecmd \"" + args.command + "\" \"" + head_tail[1] + "\"")
    os.chdir(cwd)
