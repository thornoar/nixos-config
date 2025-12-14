#!/usr/bin/env python

# `parsecmd "cmd % arg2 %" arg1` is rewritten to `cmd arg1 arg2 arg1`. This script is useful when you need to pass a long file path more than once.

import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("command", type = str, help = "command to execute")
parser.add_argument("argument", type = str, help = "argument to execute command with")
args = parser.parse_args()

cmd = args.command.replace("%", "\""+args.argument+"\"")
os.system(cmd)
