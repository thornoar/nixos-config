#!/usr/bin/env bash
prog="$1"
pkill $prog && $prog >/dev/null 2>&1 &
