#!/usr/bin/env python
import sys
import os

packages = ""
for i in range(1, len(sys.argv)):
    packages += " nixpkgs#" + sys.argv[i]

os.system("nix shell" + packages)
