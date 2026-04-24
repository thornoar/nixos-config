#!/usr/bin/env python

# Requires `vim`, `mpv`, and `yt-dlp` to be available in $PATH

import os
import subprocess
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("url", type = str, help = "the url of a YouTube video")
parser.add_argument("-i", "--id", type = str, default = "", help = "the id of a YouTube video")
parser.add_argument("-q", "--query", type = str, default = "", help = "a search query")
parser.add_argument("-d", "--download", action = "store_true", help = "download the video instead of streaming")
parser.add_argument("-t", "--ext", type = str, default = "mp4", help = "the extension of the video/audio, when downloading")
parser.add_argument("-c", "--cache", action = "store_true", help = "retrieve video information from cache")
parser.add_argument("-x", "--clear", action = "store_true", help = "delete all items from cache")
parser.add_argument("-e", "--edit", action = "store_true", help = "edit the cache file")
parser.add_argument("-s", "--subs", action = "store_true", help = "whether to also download subtitles")
parser.add_argument("-p", "--path", type = str, default = "~/media/youtube", help = "the directory to save the videos to")
args = parser.parse_args()

# Creating the cache file if it doesn't exist
cache_dir = os.environ["XDG_CACHE_HOME"] + "/yt"
cache_name = "links.cache"
cache_full_name = cache_dir + "/" + cache_name
if not os.path.exists(cache_full_name):
    print("Cache file does not exist, creating...")
    os.system("mkdir -p " + cache_dir)
    os.system("touch " + cache_full_name)

# Editing the cache file when the corresponding flag is set
if args.edit:
    os.system("vim " + cache_full_name)
    exit(0)

# Produce a URL from a YouTube video ID
def id2url (id: str):
    return "https://www.youtube.com/watch?v=" + id

# Given a dictionary of video data, prompt the user to select some of them,
# returning a list of URLs
def select_video (d: dict):
    print("Choose a video to " + (args.download and "download:" or "watch:"))
    for k, v in d.items():
        print(str(k) + ". " + v[1] + " - " + v[2])
    print("Your choices: ", end = "")
    try:
        index_str = input()
        if (index_str == "a"):
            return list(map(lambda v: id2url(v[0]), d.values()))
        return list(map(lambda str: id2url(d[int(str)][0]), index_str.split(",")))
    except:
        print("\nExiting...")
        exit(0)

# Obtaining the links of videos to stream/download
links: list[str] = []
if (len(args.url) > 0):
    links = [args.url]
elif (len(args.id) > 0):
    links = [id2url(args.id)]
elif args.clear:
    cf = open(cache_full_name, "r+")
    print("Clearing cache...")
    cf.seek(0)
    cf.truncate()
    cf.close()
    exit(0)
elif args.cache:
    cf = open(cache_full_name, "r")
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
    links = select_video(videos)
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

    cf = open(cache_full_name, "r")
    ids = []
    for line in cf:
        ids.append(line.rstrip().split(";;")[0])
    cf.close()
    cf = open(cache_full_name, "a")
    for v in videos.values():
        if (not v[0] in ids):
            cf.write(v[0] + ";;" + v[1] + ";;" + v[2] + "\n")
    cf.close()
    
    links = select_video(videos)

if (len(links) > 0):
    print("Using the following links:")
    for link in links:
        print("- " + link)
else:
    print("No links, exiting...")
    exit(0)

if (args.download):
    yt_dlp_flags = "--output \"" + args.path + "/[%(id)s] %(uploader)s - %(title)s.%(ext)s\"" + " -t" + args.ext + " "
    if args.subs:
        yt_dlp_flags += "--write-subs --sub-lang en "
    for link in links:
        os.system("yt-dlp " + yt_dlp_flags + link)
else:
    for link in links:
        os.system("mpv " + link)
