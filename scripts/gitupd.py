#!/usr/bin/env python

import os
import subprocess
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-t", "--target", type = str, default = ".", help = "specify the git directory")
parser.add_argument("-s", "--skip", action = "store_true", help = "whether to skip the check and push directly")
parser.add_argument("--pull", action = "store_true", help = "whether to only pull remote changes")
parser.add_argument("--push", action = "store_true", help = "whether to only push local changes")
args = parser.parse_args()

def run_command (command):
    return subprocess.run(command, capture_output = True, text = True).stdout.strip()

def call (str):
    if 0 != os.system(str):
        print("\033[31m> Failed to complete.\033[0m") #]]
        exit(1)

try:
    if (subprocess.call(['git', '-C', args.target, 'status'], stderr = subprocess.STDOUT, stdout = open(os.devnull, 'w')) == 0):
        if args.target == ".":
            args.target = os.popen("git rev-parse --show-toplevel").read().strip()
        cwd = os.getcwd()
        os.chdir(args.target)

        branch = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"])
        if (not args.skip):
            print("\033[34m> Updating remote branch...\033[0m") #]]
            call("git remote update")
        print("\033[34m> Checking local repository on branch \033[33m" + branch + "\033[34m...\033[0m") #]]]]
        status = run_command(["git", "status"])
        if ("branch is up to date" in status):
            if args.pull:
                print("\033[34m> Nothing to pull.\033[0m") #]]
                os.chdir(cwd)
                exit(0)
            print("branch is \033[1;94mup to date\033[0m with remote repository") #]]
            if ("working tree clean" in status):
                print("local working tree \033[1;94mclean\033[0m") #]]
            else:
                print("local changes:")
                call("git status -s")
                print("\033[33mcommit message:\033[0m") #]]
                message = input()
                if message != "skip":
                    print("\033[34m> Pushing local changes...\033[0m") #]]
                    call("git add . && git commit -m \"" + message +"\" && git push")
                else:
                    print("\033[34m> Skipping...\033[0m")
        else:
            print("local branch falls \033[1;94mbehind\033[0m remote") #]]
            if ("not staged" in status):
                if args.push or args.pull:
                    print("\033[1;31merror:\033[0m Can neither push nor pull with both local and remote changes.") #]]
                    os.chdir(cwd)
                    exit(1)
                print("changes \033[1;94mnot staged\033[0m:") #]]
                call("git status -s")
                print("\033[33mcommit message:\033[0m") #]]
                message = input()
                if message != "skip":
                    print("\033[34m> Committing the changes and merging remote branch...\033[0m") #]]
                    call("git add . && git commit -m \"" + message +"\" && git merge -m \"merge\" origin/" + branch)
                    call("git push")
            elif not args.push:
                print("working tree \033[1;94mclean\033[0m") #]]
                print("\033[34m> Merging remote changes...\033[0m") #]]
                call("git fetch && git pull")

        os.chdir(cwd)
    else:
        print("\033[1;31merror:\033[0m Not a git repository.") #]]
except KeyboardInterrupt:
    print("\n\033[31m> Interrupted by user.\033[0m")
