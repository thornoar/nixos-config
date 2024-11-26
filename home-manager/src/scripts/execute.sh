#!/usr/bin/env bash

# Arrange for the temporary file to be deleted when the script terminates
trap 'rm -f "/tmp/exec.$$"' 0
trap 'exit $?' 1 2 3 15

# Create temporary file from the standard input
cat >/tmp/exec.$$

# Make the temporary file executable
chmod +x /tmp/exec.$$

# Execute the temporary file
/tmp/exec.$$ "${@}"
