#!/usr/bin/env bash

pattern="$1"
name=""
pid="$$"

while [[ "$name" != "alacritty --title Editor -e zsh -c nvimserver; br" ]]; do
    pid="$(ps -p $pid -o ppid=)"
    name="$(ps -p $pid -o command= | awk "{ print $2 }")"
    # if [[ $name == "$pattern"* ]]; then
    #     break
    # fi
done

echo $pid
