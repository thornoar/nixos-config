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
    status = run_command(["git", "status"])
    print("\033[34m> Checking git repository on branch \033[33m" + branch + "\033[34m...\033[0m") #]]]]
    if "branch is up to date" in status:
        print("branch is \033[1;94mup to date\033[0m with remote repository") #]]
        if "working tree clean" in status:
            print("local working tree \033[1;94mclean\033[0m") #]]

