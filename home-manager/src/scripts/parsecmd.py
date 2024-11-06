#!/usr/bin/env python

import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("command", type = str, help = "command to execute")
parser.add_argument("argument", type = str, help = "argument to execute command with")
args = parser.parse_args()

cmd = args.command.replace("%", "\""+args.argument+"\"")
os.system(cmd)
