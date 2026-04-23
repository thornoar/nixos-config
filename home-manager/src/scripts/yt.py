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
args = parser.parse_args()

def id2url (id: str):
    return "https://www.youtube.com/watch?v=" + id

link: str = "none"
if (len(args.url) > 0):
    link = args.url
elif (len(args.id) > 0):
    link = id2url(args.id)
elif (len(args.query) > 0):
    result = ""
    try:
        result = subprocess.run("yt-dlp \"ytsearch100:" + args.query + "\" --get-id --get-title", shell=True).stdout
    except KeyboardInterrupt:
        print(result)
        print("hello!")
    # search_proc = subprocess.Popen(
    #     "yt-dlp \"ytsearch:" + args.query + "\" --get-id --get-title",
    #     shell = True,
    #     stdout = subprocess.PIPE,
    #     stderr = subprocess.PIPE,
    #     text = True,
    #     bufsize = 1
    # )
    # for line in search_proc:
    #     print(line, end = "")
    # print(results)

print(link)
