#!/usr/bin/env bash
ifconfig wlp46s0 | grep -i mask | awk '{print $2}'| cut -f2 -d:
