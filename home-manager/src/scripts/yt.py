#!/usr/bin/env python

import os
import subprocess
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-l", "--url", type = str, default = "", help = "the url of a YouTube video")
parser.add_argument("-i", "--id", type = str, default = "", help = "the id of a YouTube video")
parser.add_argument("-q", "--query", type = str, default = "", help = "a search query")
parser.add_argument("-d", "--download", action = "store_true", help = "download the video instead of streaming")
parser.add_argument("-t", "--ext", type = str, default = "mp4", help = "the extension of the video/audio, when downloading")
parser.add_argument("-c", "--cache", action = "store_true", help = "retrieve video information from cache")
parser.add_argument("-x", "--clear", action = "store_true", help = "delete all items from cache")
args = parser.parse_args()

def id2url (id: str):
    return "https://www.youtube.com/watch?v=" + id

def select_video (d: dict):
    print("Choose a video to " + (args.download and "download:" or "watch:"))
    for k, v in d.items():
        print(str(k) + ". " + v[1] + " - " + v[2])
    index = ""
    print("Your choice: ", end = "")
    while True:
        try:
            index = input()
        except:
            print("\nExiting...")
            exit(0)
        if index.isdigit() and int(index) in d:
            index = int(index)
            break
        else:
            print("Try again: ", end = "")
    return id2url(d[index][0])

link: str = "none"
dir = os.environ["XDG_CACHE_HOME"] + "/yt"
fname = "links.cache"
fullfname = dir + "/" + fname
if not os.path.exists(fullfname):
    print("Cache file does not exist, creating...")
    os.system("mkdir -p " + dir)
    os.system("touch " + fullfname)

if (len(args.url) > 0):
    link = args.url
elif (len(args.id) > 0):
    link = id2url(args.id)
elif args.clear:
    cf = open(fullfname, "r+")
    print("Clearing cache...")
    cf.seek(0)
    cf.truncate()
    cf.close()
    exit(0)
elif args.cache:
    cf = open(fullfname, "r")
    videos: dict = {}
    counter = 1
    for line in cf:
        parts = line.rstrip().split(";;")
        videos[counter] = parts
        counter += 1
    if (counter == 1):
        print("No cached videos!")
        exit(0)
    cf.close()
    link = select_video(videos)
elif (len(args.query) > 0):
    videos: dict = {}
    counter = 0;
    sep = "-" * 60
    title = ""
    uploader = ""
    cmd = "yt-dlp \"ytsearch100:" + args.query + "\" --print title --print uploader --print id 2> /dev/null"
    proc = subprocess.Popen(
        cmd,
        stdout = subprocess.PIPE,
        stderr = subprocess.PIPE,
        shell = True,
        text = True,
        bufsize = 1,
    )
    while True:
        try:
            if (counter % 3 == 0):
                print(sep)
            line = proc.stdout.readline().rstrip()
            print(line)
            cur = line.rstrip()
            counter += 1
            if (counter % 3 == 1):
                title = cur
            elif (counter % 3 == 2):
                uploader = cur
            elif (counter % 3 == 0):
                videos[counter // 3] = [cur, uploader, title]
        except:
            proc.terminate()
            print("\n" + sep + "\n- Finished searching for videos -\n" + sep)
            break

    cf = open(fullfname, "r")
    ids = []
    for line in cf:
        ids.append(line.rstrip().split(";;")[0])
    cf.close()
    cf = open(fullfname, "a")
    for v in videos.values():
        if (not v[0] in ids):
            cf.write(v[0] + ";;" + v[1] + ";;" + v[2] + "\n")
    cf.close()
    
    link = select_video(videos)

print(link)

if (args.download):
    os.system("yt-dlp " + link + " -t" + args.ext)
else:
    os.system("mpv " + link)
