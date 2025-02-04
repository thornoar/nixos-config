#!/usr/bin/env python

import os
import fnmatch
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-r", "--recursive", action = "store_true", help = "search for files recursively in current directory")
parser.add_argument("-b", "--background", action = "store_true", help = "run commands in background")
parser.add_argument("-p", "--parse", action = "store_true", help = "use parsecmd to parse commands")
parser.add_argument("-s", "--silent", action = "store_true", help = "report the commands and files they are ran on")
parser.add_argument("-n", "--nooutput", action = "store_true", help = "suppress output from commands")
parser.add_argument("filetype", type = str)
parser.add_argument("command", type = str)
args = parser.parse_args()

CRED = "\033[31m"#]
CGREEN = "\033[32m"#]
CPURPLE = "\033[35m"#]
CBLUE = "\033[1;34m"#]
CEND = "\033[0m"#]

cwd = os.getcwd()

try:
    for root, dirnames, filenames in os.walk('.'):
        os.chdir(root)
        if (not args.recursive and root != "."):
            continue
        if args.filetype == "dir":
            for dirname in dirnames:
                os.chdir(dirname)
                if not args.silent:
                    print(CBLUE + "#" + CEND + " Executing \"" + CGREEN + args.command + CEND + "\" in " + CPURPLE + root + "/" + dirname + CEND + "...")
                os.system(args.command + (args.nooutput and " > /dev/null 2>&1" or ""))
                os.chdir("..")
        else:
            for filename in fnmatch.filter(filenames, "*."+args.filetype):
                head_tail = os.path.split(filename)
                file = root + "/" + filename
                if ".git" in file:
                    continue
                if (".lib."+args.filetype in head_tail[1]):
                    continue
                if not args.silent:
                    print(CBLUE + "#" + CEND + " Executing \"" + CGREEN + args.command + CEND + "\" on " + CPURPLE + file + CEND + "...")
                if args.parse:
                    os.system(
                        "parsecmd \"" + args.command + (args.background and '&' or '') + "\" \"" + head_tail[1] + "\"" +
                        (args.nooutput and " > /dev/null 2>&1" or "")
                    )
                else:
                    os.system(
                        args.command + " " + head_tail[1] +
                        (args.background and '&' or '') +
                        (args.nooutput and " > /dev/null 2>&1" or "")
                    )
        os.chdir(cwd)
except KeyboardInterrupt:
    print(CRED + "#" + CEND + " Interrupted by user.")
    os.chdir(cwd)
