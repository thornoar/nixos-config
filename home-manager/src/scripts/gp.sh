#!/usr/bin/env bash

sudo cat "/home/$USER/.pshash-private-keys" | pshash "$@"
