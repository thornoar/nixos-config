#!/usr/bin/env python

import os

names = []
descs = []

lst = os.popen("pactl list sinks | grep -E \"Name|Description\"").read().split("\n")
for line in lst:
    if "HDMI" in line:
        continue
    if "Name" in line:
        names.append(line.strip().removeprefix("Name: "))
    if "Description" in line:
        descs.append(line.strip().removeprefix("Description: "))

if (len(descs) != len(descs)):
    exit(1)

try:
    cur_name = os.popen("pactl get-default-sink").read().strip()
    cur = descs[names.index(cur_name)]
    sel_desc = os.popen("echo '" + "\n".join(descs) + f"' | tofi --prompt-text 'choose: ' --placeholder-text='{cur}'").read().strip()
    if sel_desc != "":
        os.system("pactl set-default-sink " + names[descs.index(sel_desc)])
except:
    os.system("notify-send \"switch-port\" \"could not switch audio output device\"")
