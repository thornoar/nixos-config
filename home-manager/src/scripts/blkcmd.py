#!/usr/bin/env python

# A script that applies the `command` to all files in the current directory that have the specified `filetype`.

import os
import fnmatch
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-r", "--recursive", action = "store_true", help = "search for files recursively in current directory")
parser.add_argument("-b", "--background", action = "store_true", help = "run commands in background")
parser.add_argument("-p", "--parse", action = "store_true", help = "use parsecmd to parse commands")
parser.add_argument("-s", "--silent", action = "store_true", help = "do not report the commands and files they are ran on")
parser.add_argument("-n", "--nooutput", action = "store_true", help = "suppress output from commands")
parser.add_argument("-c", "--noexit", action = "store_true", help = "continue execution if any command fails")
parser.add_argument("filetype", type = str)
parser.add_argument("command", type = str)
args = parser.parse_args()

CRED = "\033[31m"#]
CGREEN = "\033[32m"#]
CEND = "\033[0m"#]

cwd = os.getcwd()

def call (cmd):
    code = os.system(cmd)
    if (code > 0 and not args.noexit):
        print(CRED + "Failed to complete." + CEND)
        os.chdir(cwd)
        exit(1)

try:
    for root, dirnames, filenames in os.walk('.'):
        os.chdir(root)
        if (not args.recursive and root != "."):
            continue
        if args.filetype == "dir":
            for dirname in dirnames:
                os.chdir(dirname)
                if not args.silent:
                    print("> Executing \"" + CGREEN + args.command + CEND + "\" in " + root + "/" + dirname + ":")
                call(args.command + (args.nooutput and " > /dev/null 2>&1" or ""))
                # print()
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
                    print("> Executing \"" + CGREEN + args.command + CEND + "\" on " + file + ":")
                if args.parse:
                    call(
                        "parsecmd \"" + args.command + (args.background and '&' or '') + "\" \"" + head_tail[1] + "\"" +
                        (args.nooutput and " > /dev/null 2>&1" or "")
                    )
                else:
                    call(
                        args.command + " " + head_tail[1] +
                        (args.background and '&' or '') +
                        (args.nooutput and " > /dev/null 2>&1" or "")
                    )
        os.chdir(cwd)
except KeyboardInterrupt:
    print(CRED + "Interrupted by user." + CEND)
    os.chdir(cwd)
