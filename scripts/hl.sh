#!/usr/bin/env bash
escape=$(printf '\033')
sed "s,$2,${escape}[$1m&${escape}[0m,g"
