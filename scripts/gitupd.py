#!/usr/bin/env python

import os
import subprocess
import argparse
import contextlib

parser = argparse.ArgumentParser()
parser.add_argument("-t", "--target", type = str, default = ".", help = "specify the git directory")
parser.add_argument("-s", "--skip", action = "store_true", help = "whether to skip the check and push directly")
args = parser.parse_args()

def run_command (command):
    return subprocess.run(command, capture_output = True, text = True).stdout.strip()

@contextlib.contextmanager
def new_cd (newdir):
    cwd = os.getcwd()
    os.chdir(newdir)
    try:
        yield
    finally:
        os.chdir(cwd)

def call (str):
    if 0 != os.system(str):
        print("\033[31m> Failed to complete.\033[0m") #]]
        exit(1)

if (subprocess.call(['git', '-C', args.target, 'status'], stderr = subprocess.STDOUT, stdout = open(os.devnull, 'w')) == 0):
    if args.target == ".":
        args.target = os.popen("git rev-parse --show-toplevel").read().strip()
    with new_cd(args.target):
        branch = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"])
        if (not args.skip):
            print("\033[34m> Updating remote branch...\033[0m") #]]
            call("git remote update")
        print("\033[34m> Checking local repository on branch \033[33m" + branch + "\033[34m...\033[0m") #]]]]
        status = run_command(["git", "status"])
        if ("branch is up to date" in status):
            print("branch is \033[1;94mup to date\033[0m with remote repository") #]]
            if ("working tree clean" in status):
                print("local working tree \033[1;94mclean\033[0m") #]]
            else:
                print("local changes:")
                call("git status -s")
                print("\033[33mcommit message:\033[0m") #]]
                message = input()
                print("\033[34m> Pushing local changes...\033[0m") #]]
                call("git add . && git commit -m \"" + message +"\" && git push")
        else:
            print("local branch falls \033[1;94mbehind\033[0m remote") #]]
            if ("not staged" in status):
                print("changes \033[1;94mnot staged\033[0m:") #]]
                call("git status -s")
                print("\033[33mcommit message:\033[0m") #]]
                message = input()
                print("\033[34m> Committing the changes and merging remote branch...\033[0m") #]]
                call("git add . && git commit -m \"" + message +"\" && git merge -m \"merge\" origin/" + branch)
                call("git push")
            else:
                print("working tree \033[1;94mclean\033[0m") #]]
                print("\033[34m> Merging remote changes...\033[0m") #]]
                call("git fetch && git pull")
else:
    print("\033[1;31merror:\033[0m Not a git repository.") #]]
