#!/usr/bin/env bash

num="$1"
query="$2"

yt-dlp "ytsearch$num:$query" --get-id --get-title
