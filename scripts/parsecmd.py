#!/usr/bin/env python

def parse (cmd, arg):
    return cmd.replace("%", arg)

import os
import sys

if (len(sys.argv) >= 3):
    if "%" in sys.argv[1]:
        os.system(parse(sys.argv[1], sys.argv[2]))
    else:
        os.system(sys.argv[1] + " " + sys.argv[2])
elif (len(sys.argv) >= 2):
    os.system(sys.argv[1])
else:
    print("no command given")
