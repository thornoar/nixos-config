#!/usr/bin/env python

import os
import subprocess
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-m", "--message", type = str, default = "--", help = "message to use for commits")
args = parser.parse_args()

def run_command (command):
    result = subprocess.run(command, capture_output = True, text = True)
    output = result.stdout.strip()
    return output

# def is_git_directory (path = '.'):
#     return subprocess.call(['git', '-C', path, 'status'], stderr=subprocess.STDOUT, stdout = open(os.devnull, 'w')) == 0



if (subprocess.call(['git', '-C', '.', 'status'], stderr=subprocess.STDOUT, stdout = open(os.devnull, 'w')) == 0):
    branch = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"])
    os.system("git remote update")
    status = run_command(["git", "status"])
    print("\033[34m> Checking git repository on branch \033[33m" + branch + "\033[34m...\033[0m") #]]]]
    if "branch is up to date" in status:
        print("branch is \033[1;94mup to date\033[0m with remote repository") #]]
        if "working tree clean" in status:
            print("local working tree \033[1;94mclean\033[0m") #]]
        else:
            print("local changes:")
            os.system("git status -s")
            print("\033[34m> Pushing local changes...\033[0m") #]]
            os.system("git add . && git commit -m " + args.message +" && git push")
    elif "not staged" in status:
        print("local branch falls \033[1;91mbehind\033[0m remote") #]]
        print("changes \033[1;91mnot staged\033[0m:") #]]
        os.system("git status -s")
        print("\033[error:\033[0m Clash between local and remote changes.") #]]
    else:
        print("local branch falls \033[1;94mbehind\033[0m remote") #]]
        print("\033[34m> Pulling remote changes...\033[0m") #]]
        os.system("git fetch && git pull")
else:
    print("\033[error:\033[0m Not a git repository.") #]]
